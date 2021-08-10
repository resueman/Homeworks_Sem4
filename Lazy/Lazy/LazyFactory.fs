namespace Lazy 

type LazyFactory = 
    
    static member CreateLazy supplier = 
        new Lazy<'a>(supplier) :> ILazy<'a>

    static member CreateLockFreeConcurrentLazy supplier = 
        new LockFreeConcurrentLazy<'a>(supplier) :> ILazy<'a>

    static member CreateBarrierConcurrentLazy supplier = 
        new BarrierConcurrentLazy<'a>(supplier) :> ILazy<'a>
