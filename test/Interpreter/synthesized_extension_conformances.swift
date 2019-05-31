// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out -module-name main -swift-version 4
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

#if FOUNDATION_XCTEST
import XCTest
class TestSuper : XCTestCase { }
#else
import StdlibUnittest
class TestSuper { }
#endif

struct SInt: Codable, Equatable, Hashable {
    var x: Int
}
struct SFloat {
    var y: Float
}
extension SFloat: Equatable {}
extension SFloat: Hashable {}
extension SFloat: Codable {}

struct SGeneric<T, U> {
    var t: T
    var u: U
}
extension SGeneric: Equatable where T: Equatable, U: Equatable {}
extension SGeneric: Hashable where T: Hashable, U: Hashable {}
extension SGeneric: Codable where T: Codable, U: Codable {}

enum EGeneric<T> {
    case a(T), b(Int), c
}
extension EGeneric: Equatable where T: Equatable {}
extension EGeneric: Hashable where T: Hashable {}

enum NoValues {
    case a, b, c
}
extension NoValues: CaseIterable {}

// Cache some values, and make them all have the same width (within a type) for
// formatting niceness.
let SIOne = SInt(x: 1)
let SITwo = SInt(x: 2)

let SFOne = SFloat(y: 1.0)
let SFTwo = SFloat(y: 2.0)
let SFInf = SFloat(y: .infinity)
let SFNan = SFloat(y: .nan)

let SGOneOne = SGeneric(t: SIOne, u: SFOne)
let SGTwoOne = SGeneric(t: SITwo, u: SFOne)
let SGTwoTwo = SGeneric(t: SITwo, u: SFTwo)
let SGOneInf = SGeneric(t: SIOne, u: SFInf)
let SGOneNan = SGeneric(t: SIOne, u: SFNan)

let EGaOne: EGeneric<SInt> = .a(SIOne)
let EGaTwo: EGeneric<SInt> = .a(SITwo)
let EGbOne: EGeneric<SInt> = .b(1)
let EGbTwo: EGeneric<SInt> = .b(2)
let EGc___: EGeneric<SInt> = .c


func debugDescription<T>(_ value: T) -> String {
    if let debugDescribable = value as? CustomDebugStringConvertible {
        return debugDescribable.debugDescription
    } else if let describable = value as? CustomStringConvertible {
        return describable.description
    } else {
        return "\(value)"
    }
}

func testEquatableHashable<T: Equatable & Hashable>(cases: [Int: (T, T, Bool, Bool)]) {
    for (testLine, (lhs, rhs, equal, hashEqual)) in cases {
        expectEqual(lhs == rhs, equal,
                    "\(#file):\(testLine) LHS <\(debugDescription(lhs))> == RHS <\(debugDescription(rhs))> doesn't match <\(equal)>")

        let lhsHash = lhs.hashValue
        let rhsHash = rhs.hashValue
        expectEqual(lhsHash == rhsHash, hashEqual,
                    "\(#file):\(testLine) LHS <\(debugDescription(lhs)).hashValue> (\(lhsHash)) == RHS <\(debugDescription(rhs)).hashValue> (\(rhsHash)) doesn't match <\(hashEqual)>")
    }
}

class TestEquatableHashable : TestSuper {
    lazy var int: [Int: (SInt, SInt, Bool, Bool)] = [
      #line : (SIOne, SIOne, true, true),
      #line : (SIOne, SITwo, false, false),
      #line : (SITwo, SIOne, false, false),
      #line : (SITwo, SITwo, true, true),
    ]

    func test_SInt() {
        testEquatableHashable(cases: int)
    }

    lazy var float: [Int: (SFloat, SFloat, Bool, Bool)] = [
      #line : (SFOne, SFOne, true, true),
      #line : (SFOne, SFTwo, false, false),
      #line : (SFTwo, SFOne, false, false),
      #line : (SFTwo, SFTwo, true, true),

      #line : (SFInf, SFInf, true, true),
      #line : (SFInf, SFOne, false, false),

      // A bit-based hasher is likely to hash these to the same thing.
      #line : (SFNan, SFNan, false, true),
      #line : (SFNan, SFOne, false, false),
    ]

    func test_SFloat() {
        testEquatableHashable(cases: float)
    }

    lazy var generic: [Int: (SGeneric<SInt, SFloat>, SGeneric<SInt, SFloat>, Bool, Bool)] = [
      #line : (SGOneOne, SGOneOne, true, true),
      #line : (SGOneOne, SGTwoOne, false, false),
      #line : (SGOneOne, SGTwoTwo, false, false),

      #line : (SGTwoOne, SGTwoOne, true, true),
      #line : (SGTwoOne, SGTwoTwo, false, false),

      #line : (SGTwoTwo, SGTwoTwo, true, true),

      #line : (SGOneInf, SGOneInf, true, true),
      #line : (SGOneInf, SGOneOne, false, false),
      #line : (SGOneInf, SGTwoOne, false, false),
      #line : (SGOneInf, SGTwoTwo, false, false),

      // As above, a bit-based hasher is likely to hash these to the same thing
      #line : (SGOneNan, SGOneNan, false, true),
      #line : (SGOneNan, SGOneOne, false, false),
      #line : (SGOneNan, SGTwoOne, false, false),
      #line : (SGOneNan, SGTwoTwo, false, false),
      #line : (SGOneNan, SGOneInf, false, false)
    ]

    func test_SGeneric() {
        testEquatableHashable(cases: generic)
    }

    lazy var egeneric: [Int: (EGeneric<SInt>, EGeneric<SInt>, Bool, Bool)] = [
      #line : (EGaOne, EGaOne, true, true),
      #line : (EGaOne, EGaTwo, false, false),
      #line : (EGaOne, EGbOne, false, false),
      #line : (EGaOne, EGbTwo, false, false),
      #line : (EGaOne, EGc___, false, false),

      #line : (EGbOne, EGaOne, false, false),
      #line : (EGbOne, EGaTwo, false, false),
      #line : (EGbOne, EGbOne, true, true),
      #line : (EGbOne, EGbTwo, false, false),
      #line : (EGbOne, EGc___, false, false),

      #line : (EGc___, EGaOne, false, false),
      #line : (EGc___, EGaTwo, false, false),
      #line : (EGc___, EGbOne, false, false),
      #line : (EGc___, EGbTwo, false, false),
      #line : (EGc___, EGc___, true, true),
    ]

    func test_EGeneric() {
        testEquatableHashable(cases: egeneric)
    }
}


func expectRoundTripEquality<T : Codable>(of value: T, lineNumber: Int) where T : Equatable {
    let inf = "INF", negInf = "-INF", nan = "NaN"
    let encoder = JSONEncoder()
    encoder.nonConformingFloatEncodingStrategy = .convertToString(positiveInfinity: inf,
                                                                  negativeInfinity: negInf,
                                                                  nan: nan)
    let data: Data
    do {
        data = try encoder.encode(value)
    } catch {
        fatalError("\(#file):\(lineNumber): Unable to encode \(T.self) <\(debugDescription(value))>: \(error)")
    }

    let decoder = JSONDecoder()
    decoder.nonConformingFloatDecodingStrategy = .convertFromString(positiveInfinity: inf,
                                                                    negativeInfinity: negInf,
                                                                    nan: nan)
    let decoded: T
    do {
        decoded = try decoder.decode(T.self, from: data)
    } catch {
        fatalError("\(#file):\(lineNumber): Unable to decode \(T.self) <\(debugDescription(value))>: \(error)")
    }

    expectEqual(value, decoded, "\(#file):\(lineNumber): Decoded \(T.self) <\(debugDescription(decoded))> not equal to original <\(debugDescription(value))>")
}
class TestCodable : TestSuper {
    lazy var int: [Int: SInt] = [
      #line : SIOne,
      #line : SITwo,
    ]

    func test_SInt() {
        for (testLine, value) in int {
            expectRoundTripEquality(of: value, lineNumber: testLine)
        }
    }

    lazy var float: [Int : SFloat] = [
      #line : SFOne,
      #line : SFTwo,
      #line : SFInf,
      // This won't compare equal to itself
      // #line : SFNan
    ]
    func test_SFloat() {
        for (testLine, value) in float {
            expectRoundTripEquality(of: value, lineNumber: testLine)
        }
    }

    lazy var generic : [Int : SGeneric<SInt, SFloat>] = [
      #line : SGOneOne,
      #line : SGTwoOne,
      #line : SGTwoTwo,
      #line : SGOneInf,
      // As above, this won't compare equal to itself
      // #line : SGOneNan,
    ]
    func test_SGeneric() {
        for (testLine, value) in generic {
            expectRoundTripEquality(of: value, lineNumber: testLine)
        }
    }
}

class TestCaseIterable : TestSuper {
    func test_allCases() {
        expectEqual(NoValues.allCases, [.a, .b, .c])
    }
}

#if !FOUNDATION_XCTEST
var equatableHashable = [
  "TestEquatableHashable.test_SInt": TestEquatableHashable.test_SInt,
  "TestEquatableHashable.test_SFloat": TestEquatableHashable.test_SFloat,
  "TestEquatableHashable.test_SGeneric": TestEquatableHashable.test_SGeneric,
  "TestEquatableHashable.test_EGeneric": TestEquatableHashable.test_EGeneric,
]
var EquatableHashableTests = TestSuite("TestEquatableHashable")
for (name, test) in equatableHashable {
    EquatableHashableTests.test(name) { test(TestEquatableHashable())() }
}

var codable = [
  "TestCodable.test_SInt": TestCodable.test_SInt,
  "TestCodable.test_SFloat": TestCodable.test_SFloat,
  "TestCodable.test_SGeneric": TestCodable.test_SGeneric,
]

var CodableTests = TestSuite("TestCodable")
for (name, test) in codable {
    CodableTests.test(name) { test(TestCodable())() }
}

var caseIterable = [
  "TestCaseIterable.test_allCases": TestCaseIterable.test_allCases,
]

var CaseIterableTests = TestSuite("TestCaseIterable")
for (name, test) in caseIterable {
    CaseIterableTests.test(name) { test(TestCaseIterable())() }
}
runAllTests()
#endif
