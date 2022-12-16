public struct A {
    public var x : Int = 0
    public var y : Int = 1
    public init() {}
#if BEFORE
    public func print() {
        printThis()
    }

    public dynamic func printThis() {
        Swift.print(x)
        Swift.print(y)
    }
#else
    public func print() {
        printThis2()
    }

    public dynamic func printThis2() {
        Swift.print(x)
        Swift.print(y)
    }

#endif
}
