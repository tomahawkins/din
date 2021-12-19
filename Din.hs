{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Din
  ( main
  ) where


import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text                     as T


-- | Print out the Javascript testcases.
main :: IO ()
main = putStrLn $ unpack jsTests


-- | Skier's type: -1, 1, 2, 3, and 3+.
data SkierType
  = SkierType1Minus
  | SkierType1
  | SkierType2
  | SkierType3
  | SkierType3Plus
  deriving (Show, Enum)


-- | Skier's mass.
data Mass
  = MassA  -- ^ 22-29 lbs / 10-13 kg
  | MassB  -- ^ 30-38 lbs / 14-17 kg
  | MassC  -- ^ 39-47 lbs / 18-21 kg
  | MassD  -- ^ 48-56 lbs / 22-25 kg
  | MassE  -- ^ 57-66 lbs / 26-30 kg
  | MassF  -- ^ 67-78 lbs / 31-35 kg
  | MassG  -- ^ 79-91 lbs / 36-41 kg
  | MassH  -- ^ 92-107 lbs / 42-48 kg
  | MassI  -- ^ 108-125 lbs / 49-57 kg
  | MassJ  -- ^ 126-147 lbs / 58-66 kg
  | MassK  -- ^ 148-174 lbs / 67-78 kg
  | MassL  -- ^ 175-209 lbs / 79-94 kg
  | MassM  -- ^ 210+ lbs / 95+ kg
  deriving (Show, Enum)


-- | Skier's height.
data Height
  = HeightH  -- ^ Less than 4'10" / 148 cm
  | HeightI  -- ^ 4'11" - 5'1" / 149-157 cm
  | HeightJ  -- ^ 5'2" - 5'5" / 158-166 cm
  | HeightK  -- ^ 5'6" - 5'10" / 167-178 cm
  | HeightL  -- ^ 5'11 - 6'5" / 179-194 cm
  | HeightM  -- ^ More than 6'5" / 195 cm
  deriving (Show, Enum)


-- | Skier code: A - O.
data SkierCode
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | J
  | K
  | L
  | M
  | N
  | O
  deriving (Eq, Ord)


-- | Skier's BSL.
data Bsl
  = Bsl0  -- ^ Less than 230 mm
  | Bsl1  -- ^ 231-250 mm
  | Bsl2  -- ^ 251-270 mm
  | Bsl3  -- ^ 271-290 mm
  | Bsl4  -- ^ 291-310 mm
  | Bsl5  -- ^ 311-330 mm
  | Bsl6  -- ^ 331-350 mm
  | Bsl7  -- ^ More than 351 mm
  deriving (Show, Enum)


-- | Skier's age.
data Age
  = Age0  -- ^ 9 and under.
  | Age1  -- ^ 10 - 49.
  | Age2  -- ^ 50 and older.
  deriving (Show, Eq, Enum)


-- | A DIN vector is composed of 8 DIN values corresponding to the 8 possible BSL values.
--   The DIN table is made up of a list of DIN vectors.
newtype DinVector = DinVector
  ( Maybe Double
  , Maybe Double
  , Maybe Double
  , Maybe Double
  , Maybe Double
  , Maybe Double
  , Maybe Double
  , Maybe Double
  )


-- | The index into the DIN table: 0 - 15.
newtype Index = Index { unIndex :: Int } deriving (Num, Eq, Ord)


-- | Skier code given mass.
skierCodeMass :: Mass -> SkierCode
skierCodeMass = \case
  MassA -> A
  MassB -> B
  MassC -> C
  MassD -> D
  MassE -> E
  MassF -> F
  MassG -> G
  MassH -> H
  MassI -> I
  MassJ -> J
  MassK -> K
  MassL -> L
  MassM -> M


-- | Skier code given height.
skierCodeHeight :: Height -> SkierCode
skierCodeHeight = \case
  HeightH -> H
  HeightI -> I
  HeightJ -> J
  HeightK -> K
  HeightL -> L
  HeightM -> M


-- | Skier code given mass and height.
skierCode :: Mass -> Height -> SkierCode
skierCode m h = min (skierCodeMass m) (skierCodeHeight h)


-- | Index for skier code.
skierCodeIndex :: SkierCode -> Index
skierCodeIndex = \case
  A -> 0
  B -> 1
  C -> 2
  D -> 3
  E -> 4
  F -> 5
  G -> 6
  H -> 7
  I -> 8
  J -> 9
  K -> 10
  L -> 11
  M -> 12
  N -> 13
  O -> 14


-- | Adjust index based on age.
adjustIndexForAge :: Age -> Index -> Index
adjustIndexForAge age index = case age of
  Age0 -> index - 1
  Age1 -> index
  Age2 -> index - 1


-- | Adjust index based on skier type.
adjustIndexForSkierType :: SkierType -> Index -> Index
adjustIndexForSkierType skierType index = case skierType of
  SkierType1Minus -> index - 1
  SkierType1      -> index
  SkierType2      -> index + 1
  SkierType3      -> index + 2
  SkierType3Plus  -> index + 3


-- | Limit skier type based on mass.  NOTE 2.
limitSkierType :: Mass -> SkierType -> SkierType
limitSkierType mass skierType = case (mass, skierType) of
  (MassA, SkierType1Minus) -> SkierType1
  (MassB, SkierType1Minus) -> SkierType1
  _                        -> skierType


-- | Lookup the final DIN setting given a BSL and a DIN vector.
lookupDin :: Bsl -> DinVector -> Maybe Double
lookupDin bsl vec = case bsl of
  Bsl0 -> din0
  Bsl1 -> din1
  Bsl2 -> din2
  Bsl3 -> din3
  Bsl4 -> din4
  Bsl5 -> din5
  Bsl6 -> din6
  Bsl7 -> din7
  where DinVector (din0, din1, din2, din3, din4, din5, din6, din7) = vec


-- | The DIN table.
dinTable :: [DinVector]
dinTable =
  [ DinVector (j 0.75, j 0.75, j 0.75, n, n, n, n, n)
  , DinVector (j 1.0, j 0.75, j 0.75, j 0.75, n, n, n, n)
  , DinVector (j 1.5, j 1.25, j 1.25, j 1.0, n, n, n, n)
  , DinVector (j 2.0, j 1.75, j 1.5, j 1.5, j 1.25, n, n, n)
  , DinVector (j 2.5, j 2.25, j 2.0, j 1.75, j 1.5, j 1.5, n, n)
  , DinVector (j 3.0, j 2.75, j 2.5, j 2.25, j 2.0, j 1.75, j 1.75, n)
  , DinVector (n, j 3.5, j 3.0, j 2.75, j 2.5, j 2.25, j 2.0, n)
  , DinVector (n, n, j 3.5, j 3.0, j 3.0, j 2.75, j 2.5, n)
  , DinVector (n, n, j 4.5, j 4.0, j 3.5, j 3.5, j 3.0, n)
  , DinVector (n, n, j 5.5, j 5.0, j 4.5, j 4.0, j 3.5, j 3.0)
  , DinVector (n, n, j 6.5, j 6.0, j 5.5, j 5.0, j 4.5, j 4.0)
  , DinVector (n, n, j 7.5, j 7.0, j 6.5, j 6.0, j 5.5, j 5.0)
  , DinVector (n, n, n, j 8.5, j 8.0, j 7.0, j 6.5, j 6.0)
  , DinVector (n, n, n, j 10.0, j 9.5, j 8.5, j 8.0, j 7.5)
  , DinVector (n, n, n, j 11.5, j 11.0, j 10.0, j 9.5, j 9.0)
  , DinVector (n, n, n, n, n, j 12.0, j 11.0, j 10.5)
  ]

 where

  j = Just
  n = Nothing


-- | Calculate DIN given mass, height, skier type, age, and BSL.
din :: Mass -> Height -> SkierType -> Age -> Bsl -> Maybe Double
din mass height skierType age bsl = lookupDin bsl $ dinTable !! unIndex index4

 where

  -- Get the index given the skier code.
  index0 = skierCodeIndex $ skierCode mass height

  -- Adjust the index for the skier type and limit skier type based on NOTE 2.
  index1 = adjustIndexForSkierType (limitSkierType mass skierType) index0

  -- Adjust the index for age.
  index2 = adjustIndexForAge age index1

  -- Limit the index to the range of the table.
  index3 = max 0 $ min 15 $ index2

  -- Set the index based on NOTE 1.
  index4 | age == Age0 = index0
         | otherwise   = index3


-- | All test vectors.
testVectors :: [(Mass, Height, SkierType, Age, Bsl, Maybe Double)]
testVectors =
  [ (mass, height, skierType', age, bsl, din mass height skierType' age bsl)
  | mass <- [MassA .. MassM]
  --, let height = HeightH
  --, let skierType' = SkierType2
  --, let age = Age1
  --, let bsl = Bsl5
  , height <- [HeightH .. HeightM]
  , skierType' <- [SkierType1Minus .. SkierType3Plus]
  , age <- [Age0 .. Age2]
  , bsl <- [Bsl0 .. Bsl7]
  ]


-- | Generate all Javascript testcases.
jsTests :: Text
jsTests = T.unlines $
  [ "/*"
  , "Test vectors for ISO11088-2018, aka. alpine ski bindings DIN settings."
  , ""
  , "Tests expect the following function under test:"
  , ""
  , "  function calculateDin(mass, height, skierType, age, bsl)"
  , ""
  , "where arguments are encoded integers:"
  , ""
  , "  mass:      0-12"
  , "  height:    0-5"
  , "  skierType: 0-4"
  , "  age:       0-2"
  , "  bsl:       0-7"
  , ""
  , "that returns the resulting DIN as either a float or a null."
  , ""
  , "*/"
  , ""
  , ""
  , "// Table of (mass, height, skierType, age, bsl, din)"
  , "var testVectors ="
  , "  [ " <> T.intercalate "\n  , "
    [ "[" <> T.intercalate ", " [encode m, encode h, encode s, encode a, encode b, result d] <> "]"
    | (m, h, s, a, b, d) <- testVectors
    ]
  , "  ]"
  , ""
  , ""
  , "// Run the test on all vectors."
  , "function runTest() {"
  , "  for (let i = 0; i < testVectors.length; i++) {"
  , "    let vector = testVectors[i]"
  , "    let result = calculateDin(vector[0], vector[1], vector[2],  vector[3], vector[4])"
  , "    if (result != vector[5]) {"
  , "      console.log(\"FAIL:  calculateDin(\" + vector[0] + \", \" + vector[1] + \", \" + vector[2] + \", \" + vector[3] + \", \" + vector[4] + \")  Expected \" + vector[5] + \", but got \" + result + \".\")"
  , "    }"
  , "  }"
  , "}"
  , ""
  ]

  where

  result :: Maybe Double -> Text
  result = \case
    Nothing -> "null"
    Just v -> showT v


showT :: Show a => a -> Text
showT = pack . show


class Encode a where
  encode :: a -> Text

instance Encode Mass where
  encode = \case
    MassA -> "0"
    MassB -> "1"
    MassC -> "2"
    MassD -> "3"
    MassE -> "4"
    MassF -> "5"
    MassG -> "6"
    MassH -> "7"
    MassI -> "8"
    MassJ -> "9"
    MassK -> "10"
    MassL -> "11"
    MassM -> "12"

instance Encode Height where
  encode = \case
    HeightH -> "0"
    HeightI -> "1"
    HeightJ -> "2"
    HeightK -> "3"
    HeightL -> "4"
    HeightM -> "5"

instance Encode SkierType where
  encode = \case
    SkierType1Minus -> "0"
    SkierType1      -> "1"
    SkierType2      -> "2"
    SkierType3      -> "3"
    SkierType3Plus  -> "4"

instance Encode Age where
  encode = \case
    Age0 -> "0"
    Age1 -> "1"
    Age2 -> "2"

instance Encode Bsl where
  encode = \case
    Bsl0 -> "0"
    Bsl1 -> "1"
    Bsl2 -> "2"
    Bsl3 -> "3"
    Bsl4 -> "4"
    Bsl5 -> "5"
    Bsl6 -> "6"
    Bsl7 -> "7"


