module InterestIsInteresting

let interestRate (balance: decimal): single =
    match balance with
    | b when b < 0.0m -> 3.213f
    | b when b < 1000m -> 0.5f
    | b when b < 5000m -> 1.621f
    | _ -> 2.475f

let interest (balance: decimal): decimal = balance * decimal (interestRate balance * 0.01f)

let annualBalanceUpdate(balance: decimal): decimal = balance + interest balance

let amountToDonate(balance: decimal) (taxFreePercentage: float): int =
   if balance > 0.0m
   then int (balance * decimal taxFreePercentage * 0.02m)
   else 0