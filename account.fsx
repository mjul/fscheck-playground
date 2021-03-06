// Test-drive FsCheck via an accounting example
// Property based testing

#I @"./packages"
#r "FsCheck/lib/net45/FsCheck.dll"

open FsCheck


// Model
type Amount = Amount of decimal
type Transaction =
  | Debit of Amount
  | Credit of Amount

type Account = { transactions: seq<Transaction> }

let emptyAccount =
  {transactions=[]}

let balance (account:Account) : Amount =
  let delta (tx:Transaction) =
    match tx with
      | Debit (Amount amt) -> -amt
      | Credit (Amount amt) -> amt
  account.transactions
  |> Seq.map delta
  |> Seq.reduce (+)
  |> Amount

let appendTransaction (tx:Transaction) (account:Account) =
  let tx' = Seq.append account.transactions [tx]
  {account with transactions = tx'}

let deposit (amt:Amount) account =
  if amt > Amount(0m) then
    Some (appendTransaction (Credit amt) account)
  else
    None

let withdraw (amt:Amount) account =
  if amt >= Amount(0m) then
    if balance account >= amt then
      Some (appendTransaction (Debit amt) account)
    else
      None
  else
    None


// Now for the testing
let ``balance of account with a single deposit is the amount deposited`` (a:decimal) =
  // conditional invariant for positive values
  a > 0m ==>
    let amount = Amount(a)
    let optacc' = deposit amount emptyAccount
    match optacc' with
      | Some acc' -> balance acc' = amount
      | None -> false

Check.Quick ``balance of account with a single deposit is the amount deposited``


// Create an Arbitrary generator for positive decimal values
// This is used as a strategy by FsCheck to create randomized input values

// Here we use a special type with pattern matching as recommended in
// the FsCheck Tips and Tricks

type PositiveDecimal = PositiveDecimal of decimal with
  static member op_Explicit(PositiveDecimal d) = d

type ArbitraryModifiers =
    static member PositiveDecimal() = 
        Arb.from<decimal> 
        |> Arb.filter (fun d -> d > 0m) 
        |> Arb.convert PositiveDecimal decimal        

Arb.register<ArbitraryModifiers>()

// Check the generator itself
let ``generated decimals should be positive`` (PositiveDecimal d) = d > 0m
Check.Quick ``generated decimals should be positive``



// We can now rewrite the test above

let ``balance of account with a single deposit is the amount deposited (refactored)`` (PositiveDecimal a) =
    let amount = Amount(a)
    let optacc' = deposit amount emptyAccount
    match optacc' with
        | Some acc' -> balance acc' = amount
        | None -> false
Check.Quick ``balance of account with a single deposit is the amount deposited (refactored)``


// In order to test sequences we need another Arbitrary input
// generator for sequences

type ArbitrarySequenceModifiers =
    static member PositiveDecimals() =
        Arb.from<PositiveDecimal>
        |> Arb.toGen
        |> Gen.nonEmptyListOf
        |> Arb.fromGen
        |> Arb.convert List.toSeq Seq.toList
Arb.register<ArbitrarySequenceModifiers>()


let ``balance of multiple deposits should be their sum`` (amounts : seq<PositiveDecimal>) =
    let depositNext state (PositiveDecimal amount) =
        match state with
            | Some account -> deposit (Amount amount) account
            | None -> None
    let optacc = Seq.fold depositNext (Some emptyAccount) amounts
    match optacc with
        | Some acc ->
            let expectedBalance = amounts |> Seq.sumBy (fun (PositiveDecimal a) -> a) |> Amount
            balance acc = expectedBalance
        | None -> false

// The following check shows that the property is falsifiable as we can get
// overflow exceptions. FsCheck really checks the corner cases!
Check.Quick ``balance of multiple deposits should be their sum``


// We can do better with an Arbitrary number generator where we limit
// the range of the amounts generated to postive numbers less or equal to the World GDP.
let worldGdp = 514.9e12m // in DKK (source: Wolfram Alpha)

type ReasonableAmount = ReasonableAmount of Amount

type ArbitraryAmountModifiers =
    static member ReasonableAmount() =
        Arb.from<decimal>
        |> Arb.convert (fun d -> ReasonableAmount(Amount d)) (fun (ReasonableAmount (Amount a)) -> a)
        //|> Arb.convert Amount (fun (Amount x) -> x)
