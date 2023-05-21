import Test.Hspec
import CarteSpec as CS
import JeuSpec as J

main :: IO ()
main = hspec $ do
    CS.carteSpec
    J.jeuSpec