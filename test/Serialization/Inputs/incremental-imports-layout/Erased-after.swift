public struct Erased {
    private let f: (Int) -> Int
    private let pad: (Int, Int, Int) = (0, 0, 0)
    public init<T>(_ value: T, _ k: Int) {
        f = { x in (value is Int ? 0 : 0) &+ x &+ k }
    }
    public func callAsFunction(_ x: Int) -> Int { f(x) }
}
