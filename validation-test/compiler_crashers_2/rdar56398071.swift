// RUN: not --crash %target-swift-frontend -primary-file %s -emit-silgen

// REQUIRES: asserts

public protocol WrappedSignedInteger: SignedInteger where Stride == Int {
    typealias WrappedInteger = Int

    var wrappedNumber: WrappedInteger { get set }
    init(wrappedNumber: WrappedInteger)
}

public extension WrappedSignedInteger {
    typealias IntegerLiteralType = WrappedInteger.IntegerLiteralType
    typealias Magnitude = WrappedInteger.Magnitude
    typealias Words = WrappedInteger.Words

    // MARK: - Initializers -

    init<T>(_ source: T) where T : BinaryInteger {
        self.init(wrappedNumber: WrappedInteger(source))
    }

    init<T>(truncatingIfNeeded source: T) where T : BinaryInteger {
        self.init(wrappedNumber: WrappedInteger(source))
    }

    init?<T>(exactly source: T) where T : BinaryFloatingPoint {
        if let wrappedNumber = WrappedInteger(exactly: source) {
            self.init(wrappedNumber: wrappedNumber)
        } else {
            return nil
        }
    }

    init<T>(_ source: T) where T : BinaryFloatingPoint {
        self.init(wrappedNumber: WrappedInteger(source))
    }

    init<T>(clamping source: T) where T : BinaryInteger {
        self.init(wrappedNumber: WrappedInteger(source))
    }

    init?<T>(exactly source: T) where T : BinaryInteger {
        if let wrappedNumber = WrappedInteger(exactly: source) {
            self.init(wrappedNumber: wrappedNumber)
        } else {
            return nil
        }
    }

    init(integerLiteral wrappedNumber: WrappedInteger.IntegerLiteralType) {
        let wrapped = WrappedInteger(integerLiteral: wrappedNumber)
        self.init(wrappedNumber: wrapped)
    }

    // MARK: - Stride -

    func advanced(by n: Int) -> Self {
        .init(wrappedNumber: wrappedNumber + n)
    }

    func distance(to other: Self) -> Self.Stride {
        other.wrappedNumber - wrappedNumber
    }

    // MARK: - Properties -

    var magnitude: WrappedInteger.Magnitude {
        wrappedNumber.magnitude
    }

    var words: WrappedInteger.Words {
        wrappedNumber.words
    }

    var bitWidth: Int {
        wrappedNumber.bitWidth
    }

    var trailingZeroBitCount: Int {
        wrappedNumber.trailingZeroBitCount
    }

    // MARK: - Operators -

    static func <<= <RHS>(lhs: inout Self, rhs: RHS) where RHS : BinaryInteger {
        lhs.wrappedNumber <<= rhs
    }

    static func >>= <RHS>(lhs: inout Self, rhs: RHS) where RHS : BinaryInteger {
        lhs.wrappedNumber >>= rhs
    }

    static prefix func ~ (x: Self) -> Self {
        .init(wrappedNumber: ~x.wrappedNumber)
    }

    static func / (lhs: Self, rhs: Self) -> Self {
        .init(wrappedNumber: lhs.wrappedNumber / rhs.wrappedNumber)
    }

    static func /= (lhs: inout Self, rhs: Self) {
        lhs.wrappedNumber /= rhs.wrappedNumber
    }

    static func % (lhs: Self, rhs: Self) -> Self {
        .init(wrappedNumber: lhs.wrappedNumber % rhs.wrappedNumber)
    }

    static func %= (lhs: inout Self, rhs: Self) {
        lhs.wrappedNumber %= rhs.wrappedNumber
    }

    static func &= (lhs: inout Self, rhs: Self) {
        lhs.wrappedNumber &= rhs.wrappedNumber
    }

    static func |= (lhs: inout Self, rhs: Self) {
        lhs.wrappedNumber |= rhs.wrappedNumber
    }

    static func ^= (lhs: inout Self, rhs: Self) {
        lhs.wrappedNumber ^= rhs.wrappedNumber
    }

    static func + (lhs: Self, rhs: Self) -> Self {
        .init(wrappedNumber: lhs.wrappedNumber + rhs.wrappedNumber)
    }

    static func += (lhs: inout Self, rhs: Self) {
        lhs.wrappedNumber += rhs.wrappedNumber
    }

    static func - (lhs: Self, rhs: Self) -> Self {
        .init(wrappedNumber: lhs.wrappedNumber - rhs.wrappedNumber)
    }

    static func -= (lhs: inout Self, rhs: Self) {
        lhs.wrappedNumber -= rhs.wrappedNumber
    }

    static func * (lhs: Self, rhs: Self) -> Self {
        .init(wrappedNumber: lhs.wrappedNumber * rhs.wrappedNumber)
    }

    static func *= (lhs: inout Self, rhs: Self) {
        lhs.wrappedNumber *= rhs.wrappedNumber
    }

}

