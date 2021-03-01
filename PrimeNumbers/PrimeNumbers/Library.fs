namespace PrimeNumbers

module PrimeNumbers =

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

    let sequence = generatePrimes ()
    Seq.iter(fun x -> printf "%d " x) sequence
