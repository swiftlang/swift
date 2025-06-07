// RUN: %target-typecheck-verify-swift

struct SIMDArrayScalar: BinaryFloatingPoint, SIMDScalar, Codable {

    typealias Impl = Float32

    typealias RawExponent = Impl.RawExponent
    typealias FloatLiteralType = Impl.FloatLiteralType
    typealias Exponent = Impl.Exponent
    typealias Stride = Impl.Stride
    typealias IntegerLiteralType = Impl.IntegerLiteralType
    typealias RawSignificand = Impl.RawSignificand

    typealias SIMDMaskScalar = Impl.SIMDMaskScalar
    typealias SIMD2Storage = SIMDArray<SIMDArrayCount2>
    typealias SIMD4Storage = SIMDArray<SIMDArrayCount4>
    typealias SIMD8Storage = SIMDArray<SIMDArrayCount8>
    typealias SIMD16Storage = SIMDArray<SIMDArrayCount16>
    typealias SIMD32Storage = SIMDArray<SIMDArrayCount32>
    typealias SIMD64Storage = SIMDArray<SIMDArrayCount64>

    var impl: Impl

    static var exponentBitCount: Int { Impl.exponentBitCount }

    static var significandBitCount: Int { Impl.significandBitCount }

    var exponentBitPattern: RawExponent { impl.exponentBitPattern }

    var significandBitPattern: RawSignificand { impl.significandBitPattern }

    var binade: SIMDArrayScalar { SIMDArrayScalar(impl.binade) }

    var significandWidth: Int { impl.significandWidth }

    static var nan: SIMDArrayScalar { SIMDArrayScalar(Impl.nan) }

    static var signalingNaN: SIMDArrayScalar { SIMDArrayScalar(Impl.signalingNaN) }

    static var infinity: SIMDArrayScalar { SIMDArrayScalar(Impl.infinity) }

    static var greatestFiniteMagnitude: SIMDArrayScalar { SIMDArrayScalar(Impl.greatestFiniteMagnitude) }

    static var pi: SIMDArrayScalar { SIMDArrayScalar(Impl.pi) }

    var ulp: SIMDArrayScalar { SIMDArrayScalar(impl.ulp) }

    static var leastNormalMagnitude: SIMDArrayScalar { SIMDArrayScalar(Impl.leastNormalMagnitude) }

    static var leastNonzeroMagnitude: SIMDArrayScalar { SIMDArrayScalar(Impl.leastNonzeroMagnitude) }

    var sign: FloatingPointSign { impl.sign }

    var exponent: Exponent { impl.exponent }

    var significand: SIMDArrayScalar { SIMDArrayScalar(impl.significand) }

    var nextUp: SIMDArrayScalar { SIMDArrayScalar(impl.nextUp) }

    var isNormal: Bool { impl.isNormal }

    var isFinite: Bool { impl.isFinite }

    var isZero: Bool { impl.isZero }

    var isSubnormal: Bool { impl.isSubnormal }

    var isInfinite: Bool { impl.isInfinite }

    var isNaN: Bool { impl.isNaN }

    var isSignalingNaN: Bool { impl.isSignalingNaN }

    var isCanonical: Bool { impl.isCanonical }

    init() {
        impl = Impl()
    }

    init(_ impl: Impl) {
        self.impl = impl
    }

    init(floatLiteral: FloatLiteralType) {
        impl = Impl(floatLiteral: floatLiteral)
    }

    init?<T>(exactly source: T) where T: BinaryInteger {
        impl = Impl(source)
    }

    init(sign: FloatingPointSign, exponent: Float32.Exponent, significand: SIMDArrayScalar) {
        impl = Impl(sign: sign, exponent: exponent, significand: significand.impl)
    }

    init(integerLiteral value: Float32.IntegerLiteralType) {
        impl = Impl(integerLiteral: value)
    }

    init(sign: FloatingPointSign, exponentBitPattern: RawExponent, significandBitPattern: RawSignificand) {
        impl = Impl(sign: sign, exponentBitPattern: exponentBitPattern, significandBitPattern: significandBitPattern)
    }

    var magnitude: SIMDArrayScalar { SIMDArrayScalar(impl.magnitude) }

    static func * (lhs: SIMDArrayScalar, rhs: SIMDArrayScalar) -> SIMDArrayScalar {
        SIMDArrayScalar(lhs.impl * rhs.impl)
    }

    static func *= (lhs: inout SIMDArrayScalar, rhs: SIMDArrayScalar) {
        lhs.impl *= rhs.impl
    }

    static func + (lhs: SIMDArrayScalar, rhs: SIMDArrayScalar) -> SIMDArrayScalar {
        SIMDArrayScalar(lhs.impl + rhs.impl)
    }

    static func - (lhs: SIMDArrayScalar, rhs: SIMDArrayScalar) -> SIMDArrayScalar {
        SIMDArrayScalar(lhs.impl - rhs.impl)
    }

    static func / (lhs: SIMDArrayScalar, rhs: SIMDArrayScalar) -> SIMDArrayScalar {
        SIMDArrayScalar(lhs.impl / rhs.impl)
    }

    static func /= (lhs: inout SIMDArrayScalar, rhs: SIMDArrayScalar) {
        lhs.impl /= rhs.impl
    }

    func distance(to other: SIMDArrayScalar) -> Float32.Stride {
        impl.distance(to: other.impl)
    }

    func advanced(by n: Float32.Stride) -> SIMDArrayScalar {
        SIMDArrayScalar(impl.advanced(by: n))
    }

    func isEqual(to other: SIMDArrayScalar) -> Bool {
        impl.isEqual(to: other.impl)
    }

    func isLess(than other: SIMDArrayScalar) -> Bool {
        impl.isLess(than: other.impl)
    }

    func isLessThanOrEqualTo(_ other: SIMDArrayScalar) -> Bool {
        impl.isLessThanOrEqualTo(other.impl)
    }

    mutating func formRemainder(dividingBy other: SIMDArrayScalar) {
        impl.formRemainder(dividingBy: other.impl)
    }

    mutating func formTruncatingRemainder(dividingBy other: SIMDArrayScalar) {
        impl.formTruncatingRemainder(dividingBy: other.impl)
    }

    mutating func formSquareRoot() {
        impl.formSquareRoot()
    }

    mutating func addProduct(_ lhs: SIMDArrayScalar, _ rhs: SIMDArrayScalar) {
        impl.addProduct(lhs.impl, rhs.impl)
    }

    mutating func round(_ rule: FloatingPointRoundingRule) {
        impl.round(rule)
    }
}

protocol SIMDArrayCount {
    static var value: Int { get }
}

struct SIMDArrayCount2: SIMDArrayCount {
    static var value: Int { 2 }
}

struct SIMDArrayCount4: SIMDArrayCount {
    static var value: Int { 4 }
}

struct SIMDArrayCount8: SIMDArrayCount {
    static var value: Int { 8 }
}

struct SIMDArrayCount16: SIMDArrayCount {
    static var value: Int { 16 }
}

struct SIMDArrayCount32: SIMDArrayCount {
    static var value: Int { 32 }
}

struct SIMDArrayCount64: SIMDArrayCount {
    static var value: Int { 64 }
}

struct SIMDArray<Count: SIMDArrayCount>: SIMDStorage {
    typealias Scalar = SIMDArrayScalar

    var scalarCount: Int { Count.value }

    var storage: [Scalar]

    init() {
        storage = [SIMDArrayScalar](repeating: 0, count: Count.value)
    }

    subscript(index: Int) -> Scalar {
        get { storage[index] }
        set(newValue) { storage[index] = newValue }
    }
}
