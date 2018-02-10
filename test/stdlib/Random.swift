// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

let RandomTests = TestSuite("Random")

// Basic random numbers

RandomTests.test("basic random numbers") {
  let randomNumber1 = Int.random(in: .min ... .max)
  let randomNumber2 = Int.random(in: .min ... .max)
  expectTrue(randomNumber1 != randomNumber2)

  let randomDouble1 = Double.random(in: 0 ..< 1)
  expectTrue(randomDouble1 < 1 && randomDouble1 >= 0)
  let randomDouble2 = Double.random(in: 0 ..< 1)
  expectTrue(randomDouble2 < 1 && randomDouble2 >= 0)
  expectTrue(randomDouble1 != randomDouble2)
}

// Random integers in ranges

func integerRangeTest<T: FixedWidthInteger>(_ type: T.Type) 
  where T.Stride: SignedInteger, T.Magnitude: UnsignedInteger {
    
  let testRange = 0 ..< 1_000
  var integerSet: Set<T> = []
  
  // min open range
  let minOpenRange = T.min ..< (T.min + 10)
  for _ in testRange {
    let random = T.random(in: minOpenRange)
    expectTrue(minOpenRange.contains(random))
    integerSet.insert(random)
  }
  expectTrue(integerSet == Set(T.min ..< (T.min + 10)))
  integerSet.removeAll()
  
  // min closed range
  let minClosedRange = T.min ... (T.min + 10)
  for _ in testRange {
    let random = T.random(in: minClosedRange)
    expectTrue(minClosedRange.contains(random))
    integerSet.insert(random)
  }
  expectTrue(integerSet == Set(T.min ... (T.min + 10)))
  integerSet.removeAll()
  
  // max open range
  let maxOpenRange = (T.max - 10) ..< T.max
  for _ in testRange {
    let random = T.random(in: maxOpenRange)
    expectTrue(maxOpenRange.contains(random))
    integerSet.insert(random)
  }
  expectTrue(integerSet == Set((T.max - 10) ..< T.max))
  integerSet.removeAll()
  
  // max closed range
  let maxClosedRange = (T.max - 10) ... T.max
  for _ in testRange {
    let random = T.random(in: maxClosedRange)
    expectTrue(maxClosedRange.contains(random))
    integerSet.insert(random)
  }
  expectTrue(integerSet == Set((T.max - 10) ... T.max))
}

RandomTests.test("random integers in ranges") {
  integerRangeTest(Int8.self)
  integerRangeTest(Int16.self)
  integerRangeTest(Int32.self)
  integerRangeTest(Int64.self)
  integerRangeTest(UInt8.self)
  integerRangeTest(UInt16.self)
  integerRangeTest(UInt32.self)
  integerRangeTest(UInt64.self)
}

// Random floating points in ranges

func floatingPointRangeTest<T: BinaryFloatingPoint>(_ type: T.Type) 
  where T.RawSignificand: FixedWidthInteger,
        T.RawSignificand.Stride: SignedInteger & FixedWidthInteger,
        T.RawSignificand.Magnitude: UnsignedInteger {
          
  let testRange = 0 ..< 1_000
  
  // open range
  let openRange: Range<T> = 0.0 ..< 10.0
  for _ in testRange {
    let random = T.random(in: openRange)
    expectTrue(openRange.contains(random))
  }
  
  // closed range
  let closedRange: ClosedRange<T> = 0.0 ... 10.0
  for _ in testRange {
    let random = T.random(in: closedRange)
    expectTrue(closedRange.contains(random))
  }
}

RandomTests.test("random floating points in ranges") {
  floatingPointRangeTest(Float.self)
  floatingPointRangeTest(Double.self)
  floatingPointRangeTest(Float80.self)
}

// Random Elements from collection

RandomTests.test("random elements from collection") {
  let greetings = ["hello", "hi", "hey", "hola", "what's up"]
  for _ in 0 ..< 1_000 {
    let randomGreeting = greetings.random()
    expectNotNil(randomGreeting)
    expectTrue(greetings.contains(randomGreeting!))
  }
}

// Shuffle

RandomTests.test("shuffling") {
  var alphabet = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l",
                  "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x",
                  "y", "z"]
  for _ in 0 ..< 1_000 {
    let newAlphabet = alphabet.shuffled()
    expectTrue(newAlphabet != alphabet)
    alphabet = newAlphabet
  }
}

// Different RNGS

public class LCRNG: RandomNumberGenerator {
  private var state: UInt64
  private static let m: UInt64 = 1 << 48
  private static let a: UInt64 = 25214903917
  private static let c: UInt64 = 11

  public init(seed: UInt64) {
    self.state = seed
  }
  
  private func next() -> UInt32 {
    state = (LCRNG.a &* state &+ LCRNG.c) % LCRNG.m
    return UInt32(truncatingIfNeeded: state >> 15)
  }
  
  public func next() -> UInt64 {
    return UInt64(next() as UInt32) << 32 | UInt64(next() as UInt32)
  }
}

RandomTests.test("different random number generators") {
  // 0 = first pass array, 1 = second pass array
  var intPasses: [[Int]] = [[], []]
  var doublePasses: [[Double]] = [[], []]
  var boolPasses: [[Bool]] = [[], []]
  var collectionPasses: [[Int]] = [[], []]
  var shufflePasses: [[[Int]]] = [[], []]
  
  for i in 0 ..< 2 {
    let seed: UInt64 = 1234567890
    let rng = LCRNG(seed: seed)
    
    for _ in 0 ..< 1_000 {
      let randomInt = Int.random(in: 0 ... 100, using: rng)
      intPasses[i].append(randomInt)
      
      let randomDouble = Double.random(in: 0 ..< 1, using: rng)
      doublePasses[i].append(randomDouble)
      
      let randomBool = Bool.random(using: rng)
      boolPasses[i].append(randomBool)
      
      let randomIntFromCollection = Array(0 ... 100).random(using: rng)
      expectNotNil(randomIntFromCollection)
      collectionPasses[i].append(randomIntFromCollection!)
      
      let randomShuffledCollection = Array(0 ... 100).shuffled(using: rng)
      shufflePasses[i].append(randomShuffledCollection)
    }
  }
  
  expectTrue(intPasses[0] == intPasses[1])
  expectTrue(doublePasses[0] == doublePasses[1])
  expectTrue(boolPasses[0] == boolPasses[1])
  expectTrue(collectionPasses[0] == collectionPasses[1])
  expectTrue(shufflePasses[0] == shufflePasses[1])
}

// Uniform Distribution

func chi2Test(_ samples: [Double]) -> Bool {
  precondition(samples.count == 50, "confidence interval requires 50 samples")
  let expected = samples.reduce(0, +) / Double(samples.count)
  let cvLow = 23.983 // 0.1% with a degree of freedom of (50 - 1)
  let cvHigh = 85.351 // 99.9% with a degree of freedom of (50 - 1)
  let chi2 = samples.map {
    (($0 - expected) * ($0 - expected)) / expected
  }.reduce(0, +)

  if chi2 < cvLow || chi2 > cvHigh {
    return false
  } else {
    return true
  }
}

RandomTests.test("uniform distribution") {
  let upperBound = 50
  let numberOfTrials = 500_000
  var array = [Double](repeating: 0.0, count: upperBound)
  for _ in 0 ..< numberOfTrials {
    let randomIndex = Int.random(in: 0 ..< upperBound)
    array[randomIndex] += 1.0
  }

  expectTrue(chi2Test(array))
}

runAllTests()