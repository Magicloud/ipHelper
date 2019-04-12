{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad.Par hiding (parMap)
import qualified Control.Monad.Par as Par (parMap)
import           Data.Bits
import           Data.Either
import           Data.List
import           Data.Word
import           GHC.Generics
import           GHC.RTS.Flags
import           System.Environment
import           System.IO
import           Text.RE.PCRE
import           Text.Regex.Base

data IPRange = IPRange { iprStart  :: Word32
                       , iprLength :: Word32 }
             deriving (Generic)
instance NFData IPRange
instance Show IPRange where
  show (IPRange s l) = intercalate "/"
    [ intercalate "." $ map show $ reverse $
      unfoldr (\s_ -> if s_ == 0
                        then Nothing
                        else Just (s_ .&. 255, shiftR s_ 8)) s
    , show $ popCount $ maxBound - l + 1 ]

data Relation a = Overlap a a
                | Join a
                | Beside a a
                | Single a
                deriving (Generic)
instance (NFData a) => NFData (Relation a)

cut :: Int -> [a] -> [[a]]
cut l as = unfoldr (\as' -> if null as'
  then Nothing
  else Just $ splitAt (length as `div` l) as') as

parMap :: (NFData b) => Int -> (a -> b) -> [a] -> [b]
parMap n f as = concat $ runPar $ Par.parMap (map f) $ cut n as
parAny :: (NFData a) => Int -> (a -> Bool) -> [a] -> Bool
parAny n f as = or $ runPar $ Par.parMap (any f) $ cut n as
parFilter :: (NFData a) => Int -> (a -> Bool) -> [a] -> [a]
parFilter n f as = concat $ runPar $ Par.parMap (filter f) $ cut n as
parZipWith :: (NFData c) => Int -> (a -> b -> c) -> [a] -> [b] -> [c]
parZipWith n f as bs = concat $ runPar $ Par.parMap (\(as', bs') -> zipWith f as' bs') $ zip (cut n as) (cut n bs)
parSortBy :: forall a. (NFData a) => Int -> (a -> a -> Ordering) -> [a] -> [a]
parSortBy n f as = deduceMerge $ runPar $ Par.parMap (sortBy f) $ cut n as
  where
    deduceMerge :: [[a]] -> [a]
    deduceMerge [cs, ds] = merge cs ds
    deduceMerge [cs] = cs
    deduceMerge [] = []
    deduceMerge xss = let (cs, ds) = splitAt 2 xss in
      merge (deduceMerge cs) (deduceMerge ds)
    merge :: [a] -> [a] -> [a]
    merge es [] = es
    merge [] fs = fs
    merge (a : es) (b : fs)
      | f a b == LT = a : merge es (b : fs)
      | otherwise = b : merge (a : es) fs
parPartitionEithers :: (NFData a, NFData b) => Int -> [Either a b] -> ([a], [b])
parPartitionEithers n es = let (ls, rs) = unzip $ runPar $ Par.parMap partitionEithers $ cut n es in
  (concat ls, concat rs)

splitOn :: (Eq a) => [a] -> a -> [[a]]
splitOn as a = groupBy (\y x -> y /= a && x /= a) as

w8ToW32 :: [Word8] -> Word32
w8ToW32 i@[_, _, _, _]= foldl1 (\a b -> shiftL a 8 .|. b) $ map fromIntegral i
w8ToW32 _ = undefined

parseIPRange :: String -> Either String IPRange
parseIPRange ipr = do
  let parser = [reBI|^(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})/(\d+|\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})$|]
      [ipaddr, mask] = mrSubList (ipr =~ parser :: MatchResult String)
      s = w8ToW32 $ map read $ filter ("." /=) $ splitOn ipaddr '.'
      m = if not $ elem '.' mask
            then shiftL maxBound $ 32 - read mask
            else w8ToW32 $ map read $ filter ("." /=) $ splitOn mask '.'
  return $ IPRange s $ maxBound - m + 1

checkSubnets :: IPRange -> IPRange -> Relation IPRange
checkSubnets a@(IPRange s1 l1) b@(IPRange s2 l2)
  | s1 + l1 > s2 = Overlap a b
  | l1 == l2 &&
    s1 + l1 == s2 &&
    s1 .&. shiftL (maxBound - l1 + 1) 1 == s1 =
      Join $ IPRange s1 (l1 * 2)
  | s1 + l1 == s2 = Beside a b
  | otherwise = Single a

deduce :: Int -> [IPRange] -> [IPRange]
deduce n rips =
  let r_ = parFilter n (\case
              Overlap _ _ -> False
              _ -> True) $ parZipWith n checkSubnets rips (tail rips ++ [IPRange 0 0])
      r = parMap n (\case
            Overlap x _ -> x
            Beside x _ -> x
            Join x -> x
            Single x -> x) r_
  in if parAny n (\case
          Join _ -> True
          _ -> False) r_
        then deduce n r
        else r

main :: IO ()
main = fromIntegral . nCapabilities <$> getParFlags >>= \n ->
  head <$> getArgs >>=
  fmap lines . readFile >>=
  return . parPartitionEithers n . parMap n parseIPRange >>= \(failed, parsed) -> do
    hPrint stderr failed
    mapM_ print $ deduce n $ parSortBy n (\a b -> compare (iprStart a) (iprStart b)) parsed
