-- Thomas' misc utils (things that are missing in the standard libraries)
module MUtils where
import Data.List(groupBy,sortBy,sort)


swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

apFst :: (a -> b) -> (a,x) -> (b,x)
apFst f (x,y) = (f x,y)

apSnd :: (a -> b) -> (x,a) -> (x,b)
apSnd f (x,y) = (x,f y)

mapFst :: (a -> b) -> [(a,x)] -> [(b,x)]
mapFst f = map (apFst f)

mapSnd :: (a -> b) -> [(x,a)] -> [(x,b)]
mapSnd f = map (apSnd f)


collectBySnd :: (Ord b) => [(a, b)] -> [([a], b)]
collectBySnd x = map pick $ groupBy eqSnd $ sortBy cmpSnd x
  where
    pick xys@((_,y):_) = ({-sort $-} map fst xys,y)
    pick _ = error "impossible: groupBy returned a list containing []"

collectByFst :: (Ord b) => [(b, a)] -> [(b, [a])]
collectByFst x = map swap $ collectBySnd $ map swap x

onSnd :: (a -> b -> c) -> (x,a) -> (y,b) -> c
onSnd f (_,y1) (_,y2) = f y1 y2

cmpSnd ::  (Ord a) => (x, a) -> (y, a) -> Ordering
cmpSnd x = onSnd compare x

eqSnd ::  (Eq a) => (x, a) -> (y, a) -> Bool
eqSnd x = onSnd (==) x


-- squeezeDups removes adjacent duplicates (cheaper than nub):
squeezeDups :: (Eq t) => [t] -> [t]
squeezeDups (r1:rrs@(r2:_)) = if r1==r2
                                 then squeezeDups rrs
                                 else r1:squeezeDups rrs
squeezeDups rs = rs

usort :: (Ord t) => [t] -> [t]
usort xs = squeezeDups (sort xs)

