import System.Console.Terminfo (Color(Yellow, Red))
data Pereche = Pereche{
    --data inseamna definire noua a tipului
    prima :: Int,
    adoua :: Int
    }
    deriving (Show, Eq)

type MyStr = [Char]
    --type imi permite sa imi fac un fel de alias
newtype Something = Some Int
    --newtype e cam aceeasi chestie cu data dar accepta un singur constructor

-------------------------------------------------
data MobileDevice = Smartphone
            | Laptop
            | Tablet
            deriving (Show,Eq)

data Culori = Yellow'
            |Purple'
            |White'
            deriving (Show,Eq)
-- ca sa mearga: Smartphone' Yellow'
data MobileDevice2 = Smartphone' Culori
            | Laptop' Culori
            | Tablet' Culori
            deriving (Show, Eq)

descriere :: MobileDevice -> String
descriere Laptop = "Acesta este un laptop de culoare roz."
descriere Tablet = "Aceasta este o tableta mov."
descriere Smartphone = "Acesta este un telefon mobil."

--1.4 functie ca sa afiseze culoarea fiecarui dispozitiv


showcolor :: MobileDevice2 -> Culori
showcolor (Smartphone' culoare) = culoare
showcolor (Laptop' culoare) = culoare
showcolor (Tablet' culoare) = culoare 

--2

data Arb = Frunza Integer | Nod Integer Arb Arb deriving (Show, Eq)





--3 expresie 