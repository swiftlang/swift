public struct Erased {
    private let f: (Int) -> Int
    public init<T>(_ value: T, _ k: Int) {
        f = { x in (value is Int ? 0 : 0) &+ x &+ k }
    }
    public func callAsFunction(_ x: Int) -> Int { f(x) }
}
