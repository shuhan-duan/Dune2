import Test.Hspec
import CarteSpec as CS
import JeuSpec as j

main :: IO ()
main = hspec $ do
    CS.carteSpec
    J.jeuSpec