import Parser (parser)

import qualified NLambda as NL

import ModelCheckerAtoms ( check )
import ModelCheckerUtils (State (..), KripkeModel)

import SyntaxUtils (Pred (..))
        

main :: IO ()
main = let phi = parser "M[ |_a . nu v0_a { b . |_c . p0_c & (&_d/=c . ~p0_d) & <>(p0_b & (&_d/=b . ~p0_d) & <>v0_c) } ]"
           states1 = NL.singleton (State 0 [NL.constant 0])
           sat1 = NL.singleton (State 0 [NL.constant 0], Pred 0 [NL.constant 0])
           trans1 = NL.singleton (State 0 [NL.constant 0], State 0 [NL.constant 0])
           model1 = (states1, trans1, sat1)
           states2 = NL.map (\a -> State 0 [a]) NL.atoms
           sat2 = NL.map (\a -> (State 0 [a], Pred 0 [a])) NL.atoms
           trans2 = NL.map (\a -> (State 0 [a], State 0 [a])) NL.atoms
           model2 = (states2, trans2, sat2)

           wrap k = State k []
           p i = wrap (i-1)
           top = wrap 10
           q i = wrap (i+10)
           bot = wrap 21
           r i = wrap (i+21)
           s i = wrap (i+25)

           a i = Pred 0 [NL.constant i]
           b i = Pred 1 [NL.constant i]

           states3list = map (`State` []) [0..29]
           preds3list = map a [0..5] ++ map b [1..4]
           trans3list = map (\i -> (p i, p (i+1))) [1..9] ++
                        map (\i -> (q i, q (i+1))) [1..9] ++
                        [(p 10, top)] ++
                        [(q 10, bot)] ++
                        map (\i -> (p (2*i+1), r i)) [1..4] ++
                        map (\i -> (q (2*i+1), s i)) [1..4] ++
                        [(top, top)]
           sat3list = map (\i -> (p (2*i), a (fromIntegral i - 1))) [1..5] ++
                      map (\i -> (q (2*i), a (fromIntegral i - 1))) [1..5] ++
                      map (\i -> (p (2*i+1), a (fromIntegral i + 1))) [0..4] ++
                      map (\i -> (q (2*i+1), a (fromIntegral i + 1))) [0..4] ++
                      map (\i -> (r i, b (fromIntegral i))) [1..4] ++
                      map (\i -> (s i, b (fromIntegral i))) [1..4] ++
                      [(top, a 5)]
           states3 = NL.fromList states3list
           trans3 = NL.fromList trans3list
           sat3 = NL.fromList sat3list
           model3 = (states3, trans3, sat3)

           states4list = map (`State` []) [0..10]
           trans4list = map (\i -> (p i, p (i+1))) [1..10] ++
                        [(p 10, top), (top, top)]
           sat4list = map (\i -> (p (2*i), a (fromIntegral i - 1))) [1..5] ++
                      map (\i -> (p (2*i+1), a (fromIntegral i + 1))) [0..4] ++
                      [(top, a 5)]
           trans4 = NL.fromList [(State 0 [], State 1 []), (State 1 [], State 2 []), (State 2 [], State 2 [])]
           sat4 = NL.fromList [(State 0 [], Pred 0 [NL.constant 0]), (State 1 [], Pred 0 [NL.constant 1]), (State 2 [], Pred 0 [NL.constant 0])]
           model4 = (NL.fromList states4list, NL.fromList trans4list, NL.fromList sat4list)

       in do print $ check model1 phi
             print $ check model2 phi
             print $ check model3 phi
             print $ check model4 phi