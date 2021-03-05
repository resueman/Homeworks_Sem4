namespace PrimeNumbers

/// <summary>Contains method that generates sequence of prime numbers</summary>
module PrimeNumbersGenerator =

    /// <summary>
    /// Generates sequence of prime numbers
    /// </summary>
    /// <returns>Sequence of prime numbers</returns>
    let generatePrimes () =
        
        let isPrime n =
            let maxContainingDivider = n |> double |> sqrt |> int
            let rec loop i =
                if n <= 1 then
                    false
                else
                    if i > maxContainingDivider then true
                    elif (n % i = 0) then false
                    else loop (i + 1)
            loop 2
        
        Seq.initInfinite(fun n -> n) |> Seq.filter(fun n -> isPrime n)
        // 0 |> Seq.unfold (fun n -> Some(n, n + 1)) |> Seq.filter(fun n -> isPrime n)
