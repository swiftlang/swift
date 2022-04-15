// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import _Differentiation
import StdlibUnittest

var PullbackTests = TestSuite("Pullback")

extension Dictionary: Differentiable where Value: Differentiable {
    public typealias TangentVector = [Key: Value.TangentVector]
    public mutating func move(by direction: TangentVector) {
        for (componentKey, componentDirection) in direction {
            func fatalMissingComponent() -> Value {
                fatalError("missing component \(componentKey) in moved Dictionary")
            }
            self[componentKey, default: fatalMissingComponent()].move(by: componentDirection)
        }
    }
    
    public var zeroTangentVectorInitializer: () -> TangentVector {
        let listOfKeys = self.keys // capturing only what's needed, not the entire self, in order to not waste memory
        func initializer() -> Self.TangentVector {
            return listOfKeys.reduce(into: [Key: Value.TangentVector]()) { $0[$1] = Value.TangentVector.zero }
        }
        return initializer
    }
}

extension Dictionary: AdditiveArithmetic where Value: AdditiveArithmetic {
    public static func + (_ lhs: Self, _ rhs: Self) -> Self {
        return lhs.merging(rhs, uniquingKeysWith: +)
    }

    public static func - (_ lhs: Self, _ rhs: Self) -> Self {
        return lhs.merging(rhs.mapValues { .zero - $0 }, uniquingKeysWith: +)
    }

    public static var zero: Self { [:] }
}

extension Dictionary where Value: Differentiable {
    // get
    @usableFromInline
    @derivative(of: subscript(_:))
    func vjpSubscriptGet(key: Key) -> (value: Value?, pullback: (Optional<Value>.TangentVector) -> Dictionary<Key, Value>.TangentVector) {
        // When adding two dictionaries, nil values are equivalent to zeroes, so there is no need to manually zero-out
        // every key's value. Instead, it is faster to create a dictionary with the single non-zero entry.
        return (self[key], { v in
            if let value = v.value {
                return [key: value]
            }
            else {
                return .zero
            }
        })
    }
 }

public extension Dictionary where Value: Differentiable {
    @differentiable(reverse)
    mutating func set(_ key: Key, to newValue: Value) {
        self[key] = newValue
    }

    @derivative(of: set)
    mutating func vjpUpdated(_ key: Key, to newValue: Value) -> (value: Void, pullback: (inout TangentVector) -> (Value.TangentVector)) {
        self.set(key, to: newValue)
        
        let forwardCount = self.count
        let forwardKeys = self.keys // may be heavy to capture all of these, not sure how to do without them though
        
        return ((), { v in
            // manual zero tangent initialization
            if v.count < forwardCount {
                v = Self.TangentVector()
                forwardKeys.forEach { v[$0] = .zero }
            }
            
            if let dElement = v[key] {
                v[key] = .zero
                return dElement
            }
            else { // should this fail?
                v[key] = .zero
                return .zero
            }
        })
    }
}


PullbackTests.test("ConcreteType") {
  func getD(from newValues: [String: Double], at key: String) -> Double? {
    if newValues.keys.contains(key) {
      return newValues[key]
    }
    return nil
  }
  
  @differentiable(reverse)
  func testFunctionD(newValues: [String: Double]) -> Double {
    return getD(from: newValues, at: "s1")!
  }

  expectEqual(pullback(at: ["s1": 1.0], of: testFunctionD)(2), ["s1" : 2.0])
}

PullbackTests.test("GenericType") {
  func getG<DataType>(from newValues: [String: DataType], at key: String) -> DataType?
  where DataType: Differentiable {
    if newValues.keys.contains(key) {
      return newValues[key]
    }
    return nil
  }

  @differentiable(reverse)
  func testFunctionG(newValues: [String: Double]) -> Double {
    return getG(from: newValues, at: "s1")!
  }

  expectEqual(pullback(at: ["s1": 1.0], of: testFunctionG)(2), ["s1" : 2.0])
}

runAllTests()
