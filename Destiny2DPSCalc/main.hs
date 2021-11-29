import BuffsDebuffs

tTap :: Int -> Int
tTap mag = mag + (mag - 1) `div` 2

fTTC :: Int -> Int
fTTC mag = 2 * mag - (2 + mag `mod` 2)

applyFuncToSplit :: (Int -> Int) -> Int -> Int -> [Int]
applyFuncToSplit func mag reserves = map func $ splitReserves mag reserves

tTapTotal :: Int -> Int -> Int
tTapTotal = (sum .) . applyFuncToSplit tTap

fTTCTotal :: Int -> Int -> Int
fTTCTotal = (sum .) . applyFuncToSplit fTTC

splitReserves :: Int -> Int -> [Int]
splitReserves mag res
  | res <= 0 = []
  | res < mag = [res]
  | otherwise = mag : splitReserves mag (res - mag)

totalDamage :: Float -> Float -> [Buff] -> [Debuff] -> Int
totalDamage dmgPerShot shots bs dbs = floor $ dmgPerShot * shots * calcBuffStack bs * calcDebuffStack dbs

main = print $ totalDamage 16430.7 (fromIntegral (tTap 23) :: Float) (buffsFromStrs ["Focusing Lens"]) $ debuffsFromStrs ["Divinity"]
