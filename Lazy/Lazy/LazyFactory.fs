namespace Lazy 

/// Creates thread-safe or thread-unsafe Lazy object, which allows to postpone creation or calculation of value
type LazyFactory = 
    
    /// Creates instance of thread-unsafe Lazy
    static member CreateLazy supplier = 
        new Lazy<'a>(supplier) :> ILazy<'a>

    /// Creates instance of thread-safe Lazy
    static member CreateLockFreeConcurrentLazy supplier = 
        new LockFreeConcurrentLazy<'a>(supplier) :> ILazy<'a>

    /// Creates instance of thread-safe Lazy
    static member CreateBarrierConcurrentLazy supplier = 
        new BarrierConcurrentLazy<'a>(supplier) :> ILazy<'a>
