/// contains methods that checks if phonebook works correctly
module Phonebook.Tests

    open Phonebook
    open FsUnit
    open NUnit.Framework

    let records = [{ Phone="8900"; Name="Natasha" }; { Phone="8908"; Name="Masha" }; 
                   { Phone="8910"; Name="Petya" }; { Phone="8910"; Name="Katya" }; 
                   { Phone="8911"; Name="Petya" }]
    let phonebook = List.fold (fun db record -> doAdd record db) [] records

    [<Test>]
    let ``Should add record to empty database`` () =
        let record = { Phone="8900"; Name="Natasha" }
        doAdd record [] |> List.contains record |> should equal true

    [<Test>]
    let ``Should add record to non-empty database`` () =
        List.iter (fun r -> List.contains r phonebook |> should equal true) phonebook

    [<Test>]
    let ``Shouldn't keep identical records`` () =
        let record = { Phone="8900"; Name="Natasha" }
        let phonebook = doAdd record []
        let phonebook' = phonebook |> doAdd record |> doAdd record
        phonebook' |> List.contains record |> should equal true
        phonebook' |> should equivalent phonebook 

    [<Test>]
    let ``Should find names by phone`` () =
        phonebook |> doFindNames "8900" |> should equivalent ["Natasha"]
        phonebook |> doFindNames "8908" |> should equivalent ["Masha"]
        phonebook |> doFindNames "8910" |> should equivalent ["Petya"; "Katya"]
        phonebook |> doFindNames "8911" |> should equivalent ["Petya"]
        [] |> doFindNames "any" |> should equivalent []


    [<Test>]
    let ``Should find phones by name`` () =
        phonebook |> doFindPhones "Natasha" |> should equivalent ["8900"]
        phonebook |> doFindPhones "Masha" |> should equivalent ["8908"]
        phonebook |> doFindPhones "Petya" |> should equivalent ["8910"; "8911"]
        phonebook |> doFindPhones "Katya" |> should equivalent ["8910"]
        [] |> doFindPhones "any" |> should equivalent []

    [<Test>]
    let ``Should restore database from file`` () =
        let phonebook' = readFromFile "restore.txt"
        match phonebook' with
        | Some db -> db |> should equivalent phonebook
        | _ -> Assert.Fail "database' is None"

    [<Test>]
    let ``Should return None if file doesn't exist on restore`` () = 
        readFromFile "file.txt" |> should equal None

    [<Test>]
    let ``Should save phonebook to file`` () =
        writeToFile "save.txt" phonebook
        let phonebook' = readFromFile "save.txt" 
        match phonebook' with
        | Some db -> db |> should equivalent phonebook
        | _ -> nameof phonebook' |> sprintf "%s is None" |> Assert.Fail
