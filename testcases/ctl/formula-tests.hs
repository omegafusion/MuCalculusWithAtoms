import qualified NLambda as NL

import SyntaxUtils ( Pred (..) )
import CTLSyntax ( Formula (..))

import Parser (parser)


removeDoubleNegations :: Formula -> Formula
removeDoubleNegations formula = case formula of
    (Negation (Negation p)) -> removeDoubleNegations p
    (Disjunction p q) -> Disjunction (removeDoubleNegations p) (removeDoubleNegations q)
    (Negation p) -> Negation (removeDoubleNegations p)
    (ExistsNext p) -> ExistsNext (removeDoubleNegations p)
    (ExistsGlobally p) -> ExistsGlobally (removeDoubleNegations p)
    (ExistsUntil p q) -> ExistsUntil (removeDoubleNegations p) (removeDoubleNegations q)
    p -> p

main :: IO ()
main =
    let formula1 = parser "C [ E F p0 ]"
        formula1expected = Right $ ExistsUntil (Boolean True) (Predicate (Pred 0 []))
        formula2 = parser "C [ A G p0 ]"
        formula2expected = Right $ Negation (ExistsUntil (Boolean True) (Negation (Predicate (Pred 0 []))))
        formula3 = parser "C [ A F p0 ]"
        formula3equiv = parser "C [ ~ E G ~p0 ]"
        formula4 = parser "C [ A (p0 U p1) ]"
        formula4equiv = parser "C [ ~E (~p1 U (~p0 & ~p1)) & ~E G ~p1 ]"
        formula5 = parser "C [ A X p0 ]"
        formula5equiv = parser "C [ ~ E X ~p0 ]"
        formula6 = parser "C [ A F A G p0 ]"
        formula6equiv = parser "C [ ~ E G E F ~p0 ]"
        formula7 = parser "C [ A G A F p0 ]"
        formula7equiv = parser "C [ ~ E F E G ~p0 ]"
    in do
        -- sometimes, the parser places double negations. so we can remove these to get an equivalent formula
        print $ formula1 `NL.eq` formula1expected;
        print $ formula2 `NL.eq` formula2expected;
        print $ formula3 `NL.eq` formula3equiv;
        print $ formula4 `NL.eq` fmap removeDoubleNegations formula4equiv;
        print $ formula5 `NL.eq` formula5equiv;
        print $ fmap removeDoubleNegations formula6 `NL.eq` formula6equiv;
        print $ fmap removeDoubleNegations formula7 `NL.eq` formula7equiv;