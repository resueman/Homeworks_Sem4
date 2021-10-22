namespace Lazy

    /// Describes the functionality which all lazy implementations should have
    type ILazy<'a> =
        abstract member Get: unit -> 'a
