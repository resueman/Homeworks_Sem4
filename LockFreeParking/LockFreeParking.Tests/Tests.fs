/// Contains tests for lock-free parking
module LockFreeParking.Tests

    open NUnit.Framework
    open FsUnit
    open LockFreeParking
    open System.Threading

    [<Test>]
    let ``Should work correctly for single automat and single parking space`` () =
        let database = ParkingSharedDatabase(1)
        database.AvailableParkingPlaceCount |> should equal 1

        let automat = ParkingAutomat(database)
        automat.CheckIn() |> should equal Response.Success
        database.AvailableParkingPlaceCount |> should equal 0

        automat.CheckIn() |> should equal Response.TryLater
        database.AvailableParkingPlaceCount |> should equal 0

        automat.CheckOut()
        database.AvailableParkingPlaceCount |> should equal 1

        automat.CheckIn() |> should equal Response.Success
        database.AvailableParkingPlaceCount |> should equal 0

    [<Test>]
    let ``Should work correctly for single automat and multiple parking spaces`` () =
        let database = ParkingSharedDatabase(10)
        database.AvailableParkingPlaceCount |> should equal 10
        
        let automat = ParkingAutomat(database)
        for i in 1 .. 10 do
            automat.CheckIn() |> should equal Response.Success
            database.AvailableParkingPlaceCount |> should equal (10 - i)
        
        for i in 1 .. 5 do
            automat.CheckIn() |> should equal Response.TryLater
            database.AvailableParkingPlaceCount |> should equal 0

        for i in 1 .. 5 do
            automat.CheckIn() |> should equal Response.TryLater
            automat.CheckOut()
            database.AvailableParkingPlaceCount |> should equal 1
            automat.CheckIn() |> should equal Response.Success
            database.AvailableParkingPlaceCount |> should equal 0

    [<Test>]
    let ``Should work correctly for several automats in single tread case`` () =
        let database = ParkingSharedDatabase(10)
        database.AvailableParkingPlaceCount |> should equal 10
        let automats = [1 .. 3] |> List.map (fun _ -> ParkingAutomat(database))
        for i in 0 .. 9 do
            automats.[i % 3].CheckIn() |> should equal Response.Success
            database.AvailableParkingPlaceCount |> should equal (9 - i)
        for i in 0 .. 9 do
            automats.[i % 3].CheckIn() |> should equal Response.TryLater
            database.AvailableParkingPlaceCount |> should equal 0
        for i in 0 .. 9 do
            automats.[i % 3].CheckOut()
            database.AvailableParkingPlaceCount |> should equal 1
            automats.[3 |> (%) (i + 1)].CheckIn() |> should equal Response.Success
            database.AvailableParkingPlaceCount |> should equal 0
            automats.[3 |> (%) i].CheckIn() |> should equal Response.TryLater            
            automats.[3 |> (%) (i + 1)].CheckIn() |> should equal Response.TryLater            
            automats.[3 |> (%) (i + 2)].CheckIn() |> should equal Response.TryLater
            database.AvailableParkingPlaceCount |> should equal 0
    
    [<Test>]
    let ``Should work correctly in case of races`` () =
        let parkingPlacesCount = 30
        let database = ParkingSharedDatabase(parkingPlacesCount)
        database.AvailableParkingPlaceCount |> should equal parkingPlacesCount
        let automats = [1 .. 3] |> List.map (fun _ -> ParkingAutomat(database))
        let checkInCount = ref 0
        let checkInFailCount = ref 0
        let checkIn autoNumber =
            async {
                let result = automats.[autoNumber % 3].CheckIn()
                if result = Response.Success then
                    Interlocked.Increment(checkInCount) |> ignore
                elif result = Response.TryLater then
                    Interlocked.Increment(checkInFailCount) |> ignore
            }

        let checkOut autoNumber =
            async {
                automats.[autoNumber % 3].CheckOut()
            }

        let autosCount = 100
        [1 .. autosCount] 
        |> Seq.map checkIn
        |> Async.Parallel
        |> Async.Ignore
        |> Async.RunSynchronously

        [1 .. parkingPlacesCount] 
        |> Seq.map checkOut
        |> Async.Parallel
        |> Async.Ignore
        |> Async.RunSynchronously

        !checkInCount |> should equal parkingPlacesCount
        !checkInFailCount |> should equal (autosCount - parkingPlacesCount)
        database.AvailableParkingPlaceCount |> should equal parkingPlacesCount
