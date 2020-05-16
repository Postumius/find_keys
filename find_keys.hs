--Resources Used
--Data.Set documentation https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Set.html
--Learn You a Haskell for Great Good! http://learnyouahaskell.com/

import Data.Set as Set


makeDependencies ls =
  fromList $
  Prelude.map (\(as, bs) -> (fromList as, fromList bs)) ls

q = makeDependencies [(['B','C'], ['A']),
                      (['A','B','D'], ['C','E']),
                      (['C','D'], ['A'])]

closure deps attrs =
  let reachable =
        Set.foldl union attrs $
        Set.map snd $
        Set.filter (\dep -> (fst dep) `isSubsetOf` attrs) deps
  in if reachable `isSubsetOf` attrs
     then attrs
     else closure deps reachable

superKeys attrs deps =
  Set.filter (\subSet -> closure deps subSet == attrs) $
  powerSet attrs

candidateKeys attrs deps =
  let superks = superKeys attrs deps
  in Set.filter
     (\superk -> Set.null $ Set.filter
       (\otherk -> otherk `isProperSubsetOf` superk) superks)
     superks
