import Test.Hspec
import Casing as Casing

main :: IO ()
main = hspec $ do
    describe "Casing.toCamelCase" $ do
        it "properly converts from camelCase" $ do
            Casing.toCamelCase "camelCase" `shouldBe` "camelCase"

        it "properly converts from PascalCase" $ do
            Casing.toCamelCase "PascalCase" `shouldBe` "pascalCase"

        it "properly converts from snake_case" $ do
            Casing.toCamelCase "snake_case" `shouldBe` "snakeCase"

        it "properly converts from SCREAMING_SNAKE_CASE" $ do
            Casing.toCamelCase "SCREAMING_SNAKE_CASE" `shouldBe` "screamingSnakeCase"

        xit "properly converts from kebab-case" $ do
            Casing.toCamelCase "kebab-case" `shouldBe` "kebabCase"

        xit "properly converts from Title Case" $ do
            Casing.toCamelCase "Title Case" `shouldBe` "titleCase"
