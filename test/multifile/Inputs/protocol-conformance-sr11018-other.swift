public protocol Proto1 {
    associatedtype Input
    func func1(_ input: Input) -> Input
}
public protocol Proto2: Proto1 {
    associatedtype Input
    func func2(_ input: Input) -> Input
}
public extension Proto2 {
    func func1(_ input: Input) -> Input { return func2(input) }
}
public struct Struct: Proto2 {
    public func func2(_ input: Float) -> Float { return input }
}
