open System
open System.Threading

type DrinkSize = Small | Medium | Large

type CoffeeType = Espresso | Latte | Cappuccino
type TeaType = GreenTea | BlackTea | HerbalTea
type JuiceType = OrangeJuice | AppleJuice | MangoJuice

type Drink =
    | Coffee of CoffeeType * DrinkSize
    | Tea of TeaType * DrinkSize
    | Juice of JuiceType * DrinkSize
    | Soda of DrinkSize
    | Milk of DrinkSize

type FoodType = Sandwich | Burger | Salad
type FruitType = Apple | Banana | Orange

type Product =
    | Drink of Drink
    | Food of FoodType
    | Fruit of FruitType

type PaymentType = Cash | CreditCard | MobilePay
type Customer = { Name: string; Payment: PaymentType }
type Order = { Customer: Customer; Product: Product; Quantity: int }

let computeDrinkPrice (drink: Drink) =
    match drink with
    | Coffee (Espresso, Small) -> 20
    | Coffee (Espresso, Medium) -> 25
    | Coffee (Espresso, Large) -> 30
    | Coffee (Latte, Small) -> 25
    | Coffee (Latte, Medium) -> 30
    | Coffee (Latte, Large) -> 35
    | Coffee (Cappuccino, Small) -> 30
    | Coffee (Cappuccino, Medium) -> 35
    | Coffee (Cappuccino, Large) -> 40
    | Tea (BlackTea, Small) -> 15
    | Tea (BlackTea, Medium) -> 20
    | Tea (BlackTea, Large) -> 25
    | Tea (GreenTea, Small) -> 20
    | Tea (GreenTea, Medium) -> 25
    | Tea (GreenTea, Large) -> 30
    | Tea (HerbalTea, Small) -> 25
    | Tea (HerbalTea, Medium) -> 30
    | Tea (HerbalTea, Large) -> 35
    | Juice (OrangeJuice, Small) -> 30
    | Juice (OrangeJuice, Medium) -> 35
    | Juice (OrangeJuice, Large) -> 40
    | Juice (AppleJuice, Small) -> 30
    | Juice (AppleJuice, Medium) -> 35
    | Juice (AppleJuice, Large) -> 40
    | Juice (MangoJuice, Small) -> 35
    | Juice (MangoJuice, Medium) -> 40
    | Juice (MangoJuice, Large) -> 45
    | Soda Small -> 15
    | Soda Medium -> 20
    | Soda Large -> 25
    | Milk Small -> 10
    | Milk Medium -> 15
    | Milk Large -> 20

let computeFoodPrice (food: FoodType) =
    match food with
    | Sandwich -> 50
    | Burger -> 70
    | Salad -> 45

let computeFruitPrice (fruit: FruitType) =
    match fruit with
    | Apple -> 10
    | Banana -> 8
    | Orange -> 12

let computeProductPrice (product: Product) =
    match product with
    | Drink d -> computeDrinkPrice d
    | Food f -> computeFoodPrice f
    | Fruit fr -> computeFruitPrice fr

let gtgVAT percent x = x + (float percent / 100.0) * x

type OrderProductMsg =
    | OrderDrink of Drink * qty:int
    | OrderFood of FoodType * qty:int
    | OrderFruit of FruitType * qty:int
    | LeaveComment of string

type gtgAgent() =
    let agent = MailboxProcessor<OrderProductMsg>.Start(fun inbox ->
        let rec messageLoop () = async {
            let! msg = inbox.Receive()
            match msg with
            | OrderDrink (drink, quantity) ->
                let basePrice = float (computeDrinkPrice drink) * float quantity
                let totalPrice = gtgVAT 7.5 basePrice
                let formattedDrink =
                    match drink with
                    | Coffee (coffeeType, size) -> sprintf "%s Coffee (%s)" (coffeeType.ToString()) (size.ToString())
                    | Tea (teaType, size) -> sprintf "%s Tea (%s)" (teaType.ToString()) (size.ToString())
                    | Juice (juiceType, size) -> sprintf "%s Juice (%s)" (juiceType.ToString()) (size.ToString())
                    | Soda size -> sprintf "Soda (%s)" (size.ToString())
                    | Milk size -> sprintf "Milk (%s)" (size.ToString())
                printfn "Please pay DKK%.2f for your %d %s drink(s). Thanks!" totalPrice quantity formattedDrink
            | OrderFood (food, quantity) ->
                let basePrice = float (computeFoodPrice food) * float quantity
                let totalPrice = gtgVAT 7.5 basePrice
                let formattedFood = sprintf "%s" (food.ToString())
                printfn "Please pay DKK%.2f for your %d %s food item(s). Thanks!" totalPrice quantity formattedFood
            | OrderFruit (fruit, quantity) ->
                let basePrice = float (computeFruitPrice fruit) * float quantity
                let totalPrice = gtgVAT 7.5 basePrice
                let formattedFruit = sprintf "%s" (fruit.ToString())
                printfn "Please pay DKK%.2f for your %d %s fruit item(s). Thanks!" totalPrice quantity formattedFruit
            | LeaveComment comment ->
                printfn "Thank you for your feedback: %s" comment
            return! messageLoop()
        }
        messageLoop()
    )
    member this.Post msg = agent.Post msg

let agent = gtgAgent()
agent.Post (OrderDrink (Coffee (Latte, Small), 2))
agent.Post (OrderFood (Burger, 1))
agent.Post (OrderFruit (Apple, 3))
agent.Post (LeaveComment "Great service!")
