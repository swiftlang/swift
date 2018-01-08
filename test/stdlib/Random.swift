// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

let RandomTests = TestSuite("Random")

RandomTests.test("random numbers") {
  let randomNumber1 = Int.random()
  let randomNumber2 = Int.random()
  expectTrue(randomNumber1 != randomNumber2)

  let randomDouble1 = Double.random()
  expectTrue(randomDouble1 < 1 && randomDouble1 > 0)
  let randomDouble2 = Double.random()
  expectTrue(randomDouble1 < 1 && randomDouble2 > 0)
  expectTrue(randomDouble1 != randomDouble2)
}

RandomTests.test("random numbers from range") {
  let range1 = 0 ..< 20
  for _ in range1 {
    let randomNumber = Int.random(in: range1)
    expectTrue(range1.contains(randomNumber))
  }

  let range2 = 0 ... 20
  for _ in range1 {
    let randomNumber = Int.random(in: range2)
    expectTrue(range2.contains(randomNumber))
  }

  let range3 = 3.0 ..< 10.0
  for _ in range1 {
    let randomNumber = Double.random(in: range3)
    expectTrue(range3.contains(randomNumber))
  }

  let range4 = 3.0 ... 10.0
  for _ in range1 {
    let randomNumber = Double.random(in: range4)
    expectTrue(range4.contains(randomNumber))
  }
}

RandomTests.test("random elements") {
  let greetings = ["hello", "hi", "hey", "hola", "what's up"]
  for _ in 0 ..< 20 {
    let randomGreeting = greetings.random()
    expectNotNil(randomGreeting)
    expectTrue(greetings.contains(randomGreeting!))
  }
}

func chi2Test(_ samples: [Double]) -> Bool {
  let upperBound = 50
  let numberOfTrials = 500_000
  let expected = Double(numberOfTrials / upperBound)
  let cvLow = 28.9 // 1% with a degree of freedom of (50 - 1)
  let cvHigh = 74.9 // 99% with a degree of freedom of (50 - 1)
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
