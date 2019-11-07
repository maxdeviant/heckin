import Test.Hspec
import Casing as Casing

makeTest transform value expected =
    it ("properly converts \"" ++ value ++ "\" to \"" ++ expected ++ "\"") $ do
        transform value `shouldBe` expected

main :: IO ()
main = hspec $ do
    describe "Casing.toCamelCase" $ do
        let test = makeTest Casing.toCamelCase

        test "camelCase" "camelCase"
        test "PascalCase" "pascalCase"
        test "snake_case" "snakeCase"
        test "SCREAMING_SNAKE_CASE" "screamingSnakeCase"
        test "kebab-case" "kebabCase"
        test "Title Case" "titleCase"
        test "XMLHttpRequest" "xmlHttpRequest"
        test "PlayerID" "playerId"
        test "IODevice" "ioDevice"
        test "NASA" "nasa"
        test "Two__Underscores" "twoUnderscores"
        test "Three___Underscores" "threeUnderscores"
        test "this-contains_ ALLKinds OfWord_Boundaries" "thisContainsAllKindsOfWordBoundaries"
