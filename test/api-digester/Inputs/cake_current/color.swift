public enum Color { case Red }

extension Color: RawRepresentable {
    public var rawValue: String { return "" }
    public init(rawValue: String) { self = .Red }
}
