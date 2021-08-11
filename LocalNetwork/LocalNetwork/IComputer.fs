namespace LocalNetwork

    /// Describes computer in local network
    type IComputer =
        abstract member OS: IOS
        abstract member IsInfected: bool
        abstract member TryToGetInfected: unit -> unit