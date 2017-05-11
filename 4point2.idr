import Data.Vect

data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  ElecScooter : Vehicle Electric
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  Unicycle : Vehicle Pedal
  Motorcycle : (fuel : Nat) -> Vehicle Petrol



wheels : Vehicle power -> Nat
wheels ElecScooter = 2
wheels Bicycle = 2
wheels (Motorcycle _) = 2
wheels Unicycle = 1
wheels (Car _) = 4
wheels (Bus _) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car _) = Car 100
refuel (Bus _) = Bus 200
refuel (Motorcycle _) = Motorcycle 50

{--
vectTake' : (k : Fin (S n)) -> Vect n a -> Vect (finToNat k) a
vectTake' FZ ys = []
vectTake' (FS x) (y::ys) = y :: (vectTake' x ys)
--}

vectTake : (m : Nat) -> Vect (m + n) a -> Vect m a
vectTake Z xs = []
vectTake (S k) (x::xs) = x :: (vectTake k xs)

--index : Fin len -> Vect len elem -> elem
--integerToFin : Integer -> (n : Nat) -> Maybe (Fin n)

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = do
                        f <- integerToFin pos n
                        pure $ (index f xs) + (index f ys)
