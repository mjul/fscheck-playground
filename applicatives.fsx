// How to use the applicative operators
// Inspired by https://github.com/swlaschin/low-risk-ways-to-use-fsharp-at-work/blob/master/TestsInFsharp/RandomDataWithFsCheck.fs

#I @"./packages"
#r "FsCheck/lib/net45/FsCheck.dll"

open System
open FsCheck

let generatePrice () =
    let scale = 100
    let generator = FsCheck.Gen.choose (100*scale, 200*scale)
    let makePrice v = decimal v / decimal(scale)
    FsCheck.Gen.map makePrice generator

// Generate some example prices
FsCheck.Gen.sample 0 5 (generatePrice())
// val it : float list = [166.81; 171.98; 178.87; 184.04; 190.93]

type Isin = Isin of string

let generateIsin () =
    let countryGen = FsCheck.Gen.elements ["DK"; "NO"; "SE"]
    let numberHiPartGen = FsCheck.Gen.choose(10000,99999)
    let numberLowPartGen = FsCheck.Gen.choose(0,99999)
    let makeIsin country highDigits lowDigits =
        sprintf "%s%05d%05d" country highDigits lowDigits
        |> Isin
    // Note: we are not filtering to only ISINs with valid check digits
    FsCheck.Gen.map3 makeIsin countryGen numberHiPartGen numberLowPartGen

// Generate some sample values
FsCheck.Gen.sample 0 2 (generateIsin())
// val it : Isin list = [Isin "NO2228286816"; Isin "DK4757642282"]


let generateDate() = 
    let minDate = DateTime(2016,10,1).ToOADate() |> int
    let maxDate = DateTime(2016,10,31).ToOADate() |> int
    let oaDateGen = FsCheck.Gen.choose(minDate,maxDate)
    let makeDate oaDate = float oaDate |> DateTime.FromOADate 
    FsCheck.Gen.map makeDate oaDateGen


type ClosePrice = {Isin:Isin; Date: DateTime; Close: decimal}

let createClosePrice isin date close =
    {Isin=isin; Date=date; Close=close}


// We can now combine the generators and the
// function with applicatives to generate 
// a generator for close prices.

let generateClosePrice =
    createClosePrice
    <!> generateIsin()
    <*> generateDate()
    <*> generatePrice()
    


// Generate some sample values
let printRandomClosePrices () =
    let count = 5
    let data = FsCheck.Gen.sample 0 count generateClosePrice
    data |> List.iter (printfn "%A")

printRandomClosePrices()

(* Example output:

{Isin = Isin "DK1226617135";
 Date = 26-10-2016 00:00:00;
 Close = 183.6M;}
{Isin = Isin "NO6448729021";
 Date = 08-10-2016 00:00:00;
 Close = 128.13M;}
{Isin = Isin "NO4837743083";
 Date = 05-10-2016 00:00:00;
 Close = 171.04M;}
{Isin = Isin "SE4866475417";
 Date = 21-10-2016 00:00:00;
 Close = 105.55M;}
{Isin = Isin "DK8830853027";
 Date = 19-10-2016 00:00:00;
 Close = 125.95M;}

   *)
