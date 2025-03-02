type DrinkSize = Small | Medium | Large

type CoffeeType = Espresso | Latte | Cappuccino
type TeaType = GreenTea | BlackTea | HerbalTea
type JuiceType = OrangeJuice | AppleJuice | MangoJuice

type Drink =
    | Coffee of CoffeeType * DrinkSize
    | Tea of TeaType * DrinkSize
    | Juice of JuiceType * DrinkSize

type FoodType = Sandwich | Salad | Pasta

type FruitType = Apple | Banana | Orange

type Product =
    | Drink of Drink
    | Food of FoodType
    | Fruit of FruitType

let getDrinkPrice (drink: Drink) =
    match drink with
    | Coffee (Espresso, Small) -> 2.5
    | Coffee (Espresso, Medium) -> 3.0
    | Coffee (Espresso, Large) -> 3.5
    | Coffee (Latte, Small) -> 3.0
    | Coffee (Latte, Medium) -> 3.5
    | Coffee (Latte, Large) -> 4.0
    | Coffee (Cappuccino, Small) -> 3.5
    | Coffee (Cappuccino, Medium) -> 4.0
    | Coffee (Cappuccino, Large) -> 4.5
    | Tea (GreenTea, Small) -> 2.0
    | Tea (GreenTea, Medium) -> 2.5
    | Tea (GreenTea, Large) -> 3.0
    | Tea (BlackTea, Small) -> 2.0
    | Tea (BlackTea, Medium) -> 2.5
    | Tea (BlackTea, Large) -> 3.0
    | Tea (HerbalTea, Small) -> 2.5
    | Tea (HerbalTea, Medium) -> 3.0
    | Tea (HerbalTea, Large) -> 3.5
    | Juice (OrangeJuice, Small) -> 3.0
    | Juice (OrangeJuice, Medium) -> 3.5
    | Juice (OrangeJuice, Large) -> 4.0
    | Juice (AppleJuice, Small) -> 3.0
    | Juice (AppleJuice, Medium) -> 3.5
    | Juice (AppleJuice, Large) -> 4.0
    | Juice (Mango, Small) -> 3.5
    | Juice (Mango, Medium) -> 4.0
    | Juice (Mango, Large) -> 4.5

let getFoodPrice (food: FoodType) =
    match food with
    | Sandwich -> 5.0
    | Salad -> 4.5
    | Pasta -> 6.5

let getFruitPrice (fruit: FruitType) =
    match fruit with
    | Apple -> 1.5
    | Banana -> 1.0
    | Orange -> 1.2

let getProductPrice (product: Product) =
    match product with
    | Drink d -> getDrinkPrice d
    | Food f -> getFoodPrice f
    | Fruit fr -> getFruitPrice fr

let order1 = Drink (Coffee (Latte, Medium))
let order2 = Food Sandwich
let order3 = Fruit Apple

printfn "Price of order1: %.2f" (getProductPrice order1)
printfn "Price of order2: %.2f" (getProductPrice order2)
printfn "Price of order3: %.2f" (getProductPrice order3)

