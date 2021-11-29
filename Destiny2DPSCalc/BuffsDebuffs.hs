module BuffsDebuffs
  ( Buff (..),
    Debuff (..),
    printBuffs,
    printDebuffs,
    calcBuffStack,
    calcDebuffStack,
    buffsFromStrs,
    debuffsFromStrs,
  )
where

import Data.Maybe (catMaybes, fromJust, fromMaybe)

data Buff
  = StackableBuff
      { buffName :: String,
        buffAmount :: Float
      }
  | GlobalBuff
      { buffName :: String,
        buffAmount :: Float
      }
  deriving (Show)

data Debuff = GlobalDebuff
  { debuffName :: String,
    debuffAmount :: Float
  }
  deriving (Show)

instance Ord Buff where
  compare a b = compare (buffAmount a) (buffAmount b)

instance Ord Debuff where
  compare a b = compare (debuffAmount a) (debuffAmount b)

instance Eq Buff where
  (==) a b = buffAmount a == buffAmount b

instance Eq Debuff where
  (==) a b = debuffAmount a == debuffAmount b

printBuff :: Buff -> String
printBuff b = buffName b ++ ": " ++ show (buffAmount b)

printDebuff :: Debuff -> String
printDebuff db = debuffName db ++ ": " ++ show (debuffAmount db)

printBuffs :: [Buff] -> String
printBuffs = unlines . map printBuff

printDebuffs :: [Debuff] -> String
printDebuffs = unlines . map printDebuff

maybeMaximum :: Ord a => [a] -> Maybe a
maybeMaximum [] = Nothing
maybeMaximum xs = Just $ maximum xs

maybeCons :: Maybe a -> [a] -> [a]
maybeCons Nothing bs = bs
maybeCons (Just b) bs = b : bs

trimGlobalBuffs :: [Buff] -> [Buff]
trimGlobalBuffs bs = maybeCons (maybeMaximum [b | b@GlobalBuff {} <- bs]) [b | b@StackableBuff {} <- bs]

trimDebuffs :: [Debuff] -> [Debuff]
trimDebuffs dbs =
  if length x == 2
    then x
    else maybeCons (maybeMaximum dbs) []
  where
    x = filter ((`elem` ["Divinity", "Particle Deconstruction"]) . debuffName) dbs

calcBuffStack :: [Buff] -> Float
calcBuffStack = multBuffs . trimGlobalBuffs
  where
    multBuffs = product . map buffAmount

calcDebuffStack :: [Debuff] -> Float
calcDebuffStack = multDebuffs . trimDebuffs
  where
    multDebuffs = product . map debuffAmount

buffsFromStrs :: [String] -> [Buff]
buffsFromStrs ss = filter (\b -> buffName b `elem` ss) buffs

debuffsFromStrs :: [String] -> [Debuff]
debuffsFromStrs ss = filter (\db -> debuffName db `elem` ss) debuffs

debuffs =
  [ GlobalDebuff {debuffName = "Shadowshot", debuffAmount = 1.30},
    GlobalDebuff {debuffName = "Hammer Strike", debuffAmount = 1.30},
    GlobalDebuff {debuffName = "Tractor Cannon", debuffAmount = 1.30},
    GlobalDebuff {debuffName = "Shattering Strike", debuffAmount = 1.30},
    GlobalDebuff {debuffName = "Withering Heat", debuffAmount = 1.30},
    GlobalDebuff {debuffName = "Particle Deconstruction", debuffAmount = 1.40},
    GlobalDebuff {debuffName = "Divinity", debuffAmount = 1.30}
  ]

buffs =
  [ GlobalBuff {buffName = "Weapons of Light", buffAmount = 1.35},
    GlobalBuff {buffName = "Well of Radiance", buffAmount = 1.25},
    GlobalBuff {buffName = "Guiding Flame", buffAmount = 1.25},
    GlobalBuff {buffName = "Sun Warrior", buffAmount = 1.20},
    GlobalBuff {buffName = "Inertia Override", buffAmount = 1.20},
    GlobalBuff {buffName = "Frontal Assault", buffAmount = 1.20},
    GlobalBuff {buffName = "Empowering Rift", buffAmount = 1.20},
    StackableBuff {buffName = "Power of Rasputin", buffAmount = 1.10},
    StackableBuff {buffName = "Font of Might", buffAmount = 1.25},
    StackableBuff {buffName = "Focusing Lens", buffAmount = 1.25}
  ]
