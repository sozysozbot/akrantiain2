import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "List is instance of Functor" $ do
    it "satisfies Functor law : fmap id == id" $
        fmap id ([1,2,3]::[Integer]) `shouldBe` [1,2,3]
    it "satisfies Functor law : fmap (g.h) f == (fmap g . fmap h) f" $
        fmap ((+1).(*2)) ([1,2,3]::[Integer]) `shouldBe` fmap (+1) (fmap (*2) [1,2,3])
