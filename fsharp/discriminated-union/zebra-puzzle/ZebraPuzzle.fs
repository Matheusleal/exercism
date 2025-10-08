module ZebraPuzzle

type Owner =    | Norwegian | Japanese  | Englishman | Spaniard    | Ukrainian
type Drink =    | Coffee    | Tea       | Milk       | OrangeJuice | Water
type House =    | Red       | Ivory     | Green      | Yellow      | Blue
type Animal =   | Zebra     | Dog       | Snail      | Fox         | Horse
type Activity = | Dance     | Paint     | Read       | Football    | Chess

type Person = { Owner: Owner; House: House; Animal: Animal; Activity: Activity; Drink: Drink; }

let persons: Person list = [
    {Owner = Norwegian;  House = Yellow; Animal = Fox;   Activity = Paint;    Drink = Water       }
    {Owner = Ukrainian;  House = Blue;   Animal = Horse; Activity = Read;     Drink = Tea         }
    {Owner = Englishman; House = Red;    Animal = Snail; Activity = Dance;    Drink = Milk        }
    {Owner = Spaniard;   House = Ivory;  Animal = Dog;   Activity = Football; Drink = OrangeJuice }
    {Owner = Japanese;   House = Green;  Animal = Zebra; Activity = Chess;    Drink = Coffee      }
]

let drinksWater: Owner =
    persons
    |> List.find (fun x -> x.Drink = Drink.Water)
    |> fun person -> person.Owner

let ownsZebra: Owner =
    persons
    |> List.find (fun x -> x.Animal = Animal.Zebra)
    |> fun person -> person.Owner
