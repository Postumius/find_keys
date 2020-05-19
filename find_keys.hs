--Resources Used
--Data.Set documentation https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Set.html

import Data.Set as Set

makeDependencies ls =
  fromList $
  Prelude.map (\(as, bs) -> (fromList as, fromList bs)) ls

closure deps attrs =
  let reachable =
        Set.foldl union attrs $
        Set.map snd $
        Set.filter (\(lhs, _) -> lhs `isSubsetOf` attrs) deps
  in if reachable `isSubsetOf` attrs
     then attrs
     else closure deps reachable

superKeys attrs deps =
  Set.filter (\subSet -> closure deps subSet == attrs) $
  powerSet attrs

candidateKeys attrs deps =
  let superKs = superKeys attrs deps
  in Set.filter
     (\superK -> Set.null $ Set.filter
       (\otherk -> otherk `isProperSubsetOf` superK) superKs)
     superKs


flatten :: (Ord a) => Set (Set a) -> Set a
flatten = Set.foldl union empty

viol2nf attrs deps =
  let candKs = candidateKeys attrs deps        
  in Set.filter
     (\attr ->
        let inCandK =
              attr `member` flatten candKs
            keysMissingOne =
              flatten $ Set.map
              (\key -> Set.map (\attr -> delete attr key) key)
              candKs
            dependsOnAll =
              and $ Set.map
              (\set -> not $ attr `member` closure deps set) $
              keysMissingOne
        in not $ inCandK || dependsOnAll)
     attrs

viol3nf attrs deps =
  let superKs = superKeys attrs deps
      candKs = candidateKeys attrs deps
  in Set.filter
     (\(lhs, rhs) ->
        let trivial = rhs `isSubsetOf` lhs
            lhsIsSuperK = lhs `member` superKs
            rhsIsPartOfCand =
              and $
              Set.map
              (\attr ->
                 attr `member` flatten candKs) $
              rhs `difference` lhs
        in not $ trivial || lhsIsSuperK || rhsIsPartOfCand)
     deps

violBcnf attrs deps =
  let superKs = superKeys attrs deps 
  in Set.filter
     (\(lhs, rhs) ->
         let trivial = rhs `isSubsetOf` lhs
             lhsIsSuperK = lhs `member` superKs
         in not $ trivial || lhsIsSuperK)
     deps


qAttrs = fromList ['A','B','C','D','E']
qDeps = makeDependencies
  [(['B','C'],     ['A']),
   (['A','B','D'], ['C','E']),
   (['C','D'],     ['A'])]
        
rAttrs = fromList ['A','B','C','D','E','F']
rDeps = makeDependencies
  [(['A','B'], ['C']),
   (['C','D'], ['B','F']),
   (['A','E'], ['C']),
   (['A','D'], ['E'])]

sAttrs = fromList ['A','B','C','D','E','F','G','H']
sDeps = makeDependencies
  [(['A','C','G'], ['B']),
   (['B'],         ['G']),
   (['C'],         ['D']),
   (['G'],         ['A']),
   (['D','E'],     ['G','C']),
   (['H'],         ['E','F']),
   (['D','E','F'], ['H'])]
