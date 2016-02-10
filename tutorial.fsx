// The Quick Start Tutorial
// See: https://fscheck.github.io/FsCheck/QuickStart.html

#I "packages/FsCheck/lib/net45"
#r "FsCheck"

open FsCheck


let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs

FsCheck.Check.Quick revRevIsOrig

let revIsOrig (xs:list<int>) = List.rev xs = xs
FsCheck.Check.Quick revIsOrig

let revRevIsOrigFloat (xs:list<float>) = List.rev(List.rev xs) = xs
FsCheck.Check.Quick revRevIsOrigFloat




