public struct Point: Equatable, Hashable {
    public var x: Int
    public var y: Int
    public init(x: Int, y: Int) { self.x = x; self.y = y }
}

public struct Temperature: Comparable {
    public var degrees: Int
    public init(degrees: Int) { self.degrees = degrees }
    public static func < (lhs: Temperature, rhs: Temperature) -> Bool {
        return lhs.degrees < rhs.degrees
    }
    public static func == (lhs: Temperature, rhs: Temperature) -> Bool {
        return lhs.degrees == rhs.degrees
    }
}
