// RUN: %target-swift-frontend %s -emit-silgen

// https://github.com/apple/swift/issues/45761

protocol P: Equatable, ExpressibleByStringLiteral {
    var uid: String { get }
    init(uid: String)
}

extension P {
    // Equatable
    public static func ==(lhs: Self, rhs: Self) -> Bool {
        return lhs.uid == rhs.uid
    }

    // ExpressibleByStringLiteral
    public init(stringLiteral value: String) {
        self.init(uid: value)
    }
    public init(unicodeScalarLiteral value: String) {
        self.init(uid: value)
    }
    public init(extendedGraphemeClusterLiteral value: String) {
        self.init(uid: value)
    }
}

struct Test: P {
    var uid: String
    static let s1: Test = "s1"
}

Test.s1 == "test"
