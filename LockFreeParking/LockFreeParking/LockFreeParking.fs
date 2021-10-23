namespace LockFreeParking

open System.Threading

/// Contains types that implements lock free parking
module LockFreeParking =
    
    type Response = Success | TryLater | Error

    /// Database storing information on the available number of parking places
    /// and carrying out registration and departure of cars
    type ParkingSharedDatabase (parkingPlaceCount: int) = 
        let availableParkingPlaceCount = ref parkingPlaceCount

        /// Indicates how much parking places are available
        member this.AvailableParkingPlaceCount with get () = !availableParkingPlaceCount
        
        /// If there are free parking spaces returns 'Registered' and check-ins the car; 
        /// otherwise returns 'FullParking', which means that driver should try to check-in later 
        member this.CheckOut () =
            let mutable currentVal = !availableParkingPlaceCount
            let mutable startVal = 0
            let mutable desiredVal = 0
            let mutable isDone = false
            while not isDone do
                startVal <- !availableParkingPlaceCount
                desiredVal <- startVal + 1
                if startVal < parkingPlaceCount then
                    currentVal <- Interlocked.CompareExchange(availableParkingPlaceCount, desiredVal, startVal)  
                    if startVal = currentVal then
                        isDone <- true
                else
                    isDone <- true

        /// Registers the departure of the car and the release of the parking space  
        member this.TryCheckIn () =
            let mutable currentVal = !availableParkingPlaceCount
            let mutable startVal = 0
            let mutable desiredVal = 0
            let mutable answer = Error
            while answer = Error do
                startVal <- !availableParkingPlaceCount
                desiredVal <- startVal - 1
               
                if startVal = 0 then 
                    answer <- TryLater       
                else 
                    currentVal <- Interlocked.CompareExchange(availableParkingPlaceCount, desiredVal, startVal)  
                    if startVal = currentVal then
                        answer <- Success
            answer

    /// Implementation of one of the many parking automats that check in and out cars of the parking
    type ParkingAutomat (db: ParkingSharedDatabase) = 
        
        /// Informs the driver if there are parking spaces. 
        /// If there are free parking spaces returns 'Success' and check-ins the car; 
        /// otherwise returns 'TryLater', which means that driver should try to check-in later
        member this.CheckIn () =
            db.TryCheckIn()

        /// Registers the departure of the car and the release of the parking space 
        member this.CheckOut () = 
            db.CheckOut()
