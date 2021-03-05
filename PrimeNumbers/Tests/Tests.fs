namespace PrimeNumbersGenerator.Tests

/// <summary>Contains tests for checking the correctness of 
/// prime numbers generator's work</summary>
module Tests =
    open FsUnit
    open NUnit.Framework
    open PrimeNumbers.PrimeNumbersGenerator

    [<Test>]
    let ``Should generate right first ten primes`` () =
        let primes = generatePrimes () |> Seq.take 10
        let s = seq { 2; 3; 5; 7; 11; 13; 17; 19; 23; 29}
        primes |> should equal s

    [<Test>]
    let ``Random peeked generated prime should be equal to expected`` () =
        let primes = generatePrimes ()
        Seq.item 668 primes |> should equal 4999
        Seq.item 238 primes |> should equal 1499
        Seq.item 233 primes |> should equal 1481
        Seq.item 197 primes |> should equal 1213
        Seq.item 107 primes |> should equal 593
        Seq.item 35 primes |> should equal 151
        Seq.item 17 primes |> should equal 61
        Seq.item 0 primes |> should equal 2
