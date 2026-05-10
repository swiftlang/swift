// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var StoreBorrowAdjTest = TestSuite("StoreBorrowAdjTest")

public struct ConstantTimeAccessor<Element>: Differentiable where Element: Differentiable, Element: AdditiveArithmetic {
    public struct TangentVector: Differentiable, AdditiveArithmetic {
        public typealias TangentVector = ConstantTimeAccessor.TangentVector
        public var _base: [Element.TangentVector]
        public var accessed: Element.TangentVector

        public init(_base: [Element.TangentVector], accessed: Element.TangentVector) {
            self._base = _base
            self.accessed = accessed
        }
    }

    @usableFromInline
    var _values: [Element]

    public var accessed: Element

    @inlinable
    @differentiable(reverse)
    public init(_ values: [Element], accessed: Element = .zero) {
        self._values = values
        self.accessed = accessed
    }

    @inlinable
    @differentiable(reverse)
    public var array: [Element] { return _values }

    @noDerivative
    public var count: Int { return _values.count }
}

public extension ConstantTimeAccessor {
    @inlinable
    @derivative(of: init(_:accessed:))
    static func _vjpInit(_ values: [Element],
                         accessed: Element = .zero)
        -> (value: ConstantTimeAccessor, pullback: (TangentVector) -> (Array<Element>.TangentVector, Element.TangentVector)) {
        return (ConstantTimeAccessor(values, accessed: accessed), { v in
            let base: Array<Element>.TangentVector
            if v._base.count < values.count {
                base = Array<Element>
                    .TangentVector(v._base + Array<Element.TangentVector>(repeating: .zero, count: values.count - v._base.count))
            }
            else {
                base = Array<Element>.TangentVector(v._base)
            }

            return (base, v.accessed)
        })
    }

    @inlinable
    @derivative(of: array)
    func vjpArray() -> (value: [Element], pullback: (Array<Element>.TangentVector) -> TangentVector) {
    func pullback(v: Array<Element>.TangentVector) -> TangentVector {
            var base: [Element.TangentVector]
            let localZero = Element.TangentVector.zero
            if v.base.allSatisfy({ $0 == localZero }) {
                base = []
            }
            else {
                base = v.base
            }
            return TangentVector(_base: base, accessed: Element.TangentVector.zero)
    }
        return (_values, pullback)
    }

    mutating func move(by offset: TangentVector) {
        self.accessed.move(by: offset.accessed)
        _values.move(by: Array<Element>.TangentVector(offset._base))
    }
}

public extension ConstantTimeAccessor.TangentVector {
    @inlinable
    static func + (lhs: Self, rhs: Self) -> Self {
        if rhs._base.isEmpty {
            return lhs
        }
        else if lhs._base.isEmpty {
            return rhs
        }
        else {
            var base = zip(lhs._base, rhs._base).map(+)
            if lhs._base.count < rhs._base.count {
                base.append(contentsOf: rhs._base.suffix(from: lhs._base.count))
            }
            else if lhs._base.count > rhs._base.count {
                base.append(contentsOf: lhs._base.suffix(from: rhs._base.count))
            }

            return Self(_base: base, accessed: lhs.accessed + rhs.accessed)
        }
    }

    @inlinable
    static func - (lhs: Self, rhs: Self) -> Self {
        if rhs._base.isEmpty {
            return lhs
        }
        else {
            var base = zip(lhs._base, rhs._base).map(-)
            if lhs._base.count < rhs._base.count {
                base.append(contentsOf: rhs._base.suffix(from: lhs._base.count).map { .zero - $0 })
            }
            else if lhs._base.count > rhs._base.count {
                base.append(contentsOf: lhs._base.suffix(from: rhs._base.count))
            }

            return Self(_base: base, accessed: lhs.accessed - rhs.accessed)
        }
    }

    @inlinable
    static var zero: Self { Self(_base: [], accessed: .zero) }
}

StoreBorrowAdjTest.test("NonZeroGrad") {
  @differentiable(reverse)
  func testInits(input: [Float]) -> Float {
    let internalAccessor = ConstantTimeAccessor(input)
    let internalArray = internalAccessor.array
    return internalArray[1]
  }

  let grad = gradient(at: [42.0, 146.0, 73.0], of: testInits)
  expectEqual(grad[1], 1.0)
}

runAllTests()
