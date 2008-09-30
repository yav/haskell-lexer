{-|
Compiling regular expressions, the new way.
-}
module CompileRegExp where
import RegExpOps
import Data.Maybe(fromJust)
import qualified Data.Map as Map
--import Trace

compile r = number (build Map.empty [r])
  where
    build _ [] = []
    build dfa (r:rs) =
       if Map.member r dfa
       then build dfa rs
       else (r,fs):build dfa' (frs++rs)
     where
       dfa' = Map.insert r fs dfa
       fs = factors r
       frs = map snd (snd fs)

    number states = map numb states
      where
        mapping = Map.fromList (zip (map fst states) [(1::Int) ..])
        num = fromJust . flip Map.lookup mapping

        numb (r,(b,edges)) = (num r,(b,[(t,num r)|(t,r)<-edges]))
