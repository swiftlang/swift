//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let ExistentialPerformance = [
  BenchmarkInfo(name: "DistinctClassFieldAccesses", runFunction: run_DistinctClassFieldAccesses, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayConditionalShift_ClassValueBuffer1", runFunction: run_ExistentialTestArrayConditionalShift_ClassValueBuffer1, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayConditionalShift_ClassValueBuffer2", runFunction: run_ExistentialTestArrayConditionalShift_ClassValueBuffer2, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayConditionalShift_ClassValueBuffer3", runFunction: run_ExistentialTestArrayConditionalShift_ClassValueBuffer3, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayConditionalShift_ClassValueBuffer4", runFunction: run_ExistentialTestArrayConditionalShift_ClassValueBuffer4, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayConditionalShift_IntValueBuffer0", runFunction: run_ExistentialTestArrayConditionalShift_IntValueBuffer0, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayConditionalShift_IntValueBuffer1", runFunction: run_ExistentialTestArrayConditionalShift_IntValueBuffer1, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayConditionalShift_IntValueBuffer2", runFunction: run_ExistentialTestArrayConditionalShift_IntValueBuffer2, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayConditionalShift_IntValueBuffer3", runFunction: run_ExistentialTestArrayConditionalShift_IntValueBuffer3, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayConditionalShift_IntValueBuffer4", runFunction: run_ExistentialTestArrayConditionalShift_IntValueBuffer4, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayMutating_ClassValueBuffer1", runFunction: run_ExistentialTestArrayMutating_ClassValueBuffer1, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayMutating_ClassValueBuffer2", runFunction: run_ExistentialTestArrayMutating_ClassValueBuffer2, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayMutating_ClassValueBuffer3", runFunction: run_ExistentialTestArrayMutating_ClassValueBuffer3, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayMutating_ClassValueBuffer4", runFunction: run_ExistentialTestArrayMutating_ClassValueBuffer4, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayMutating_IntValueBuffer0", runFunction: run_ExistentialTestArrayMutating_IntValueBuffer0, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayMutating_IntValueBuffer1", runFunction: run_ExistentialTestArrayMutating_IntValueBuffer1, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayMutating_IntValueBuffer2", runFunction: run_ExistentialTestArrayMutating_IntValueBuffer2, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayMutating_IntValueBuffer3", runFunction: run_ExistentialTestArrayMutating_IntValueBuffer3, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayMutating_IntValueBuffer4", runFunction: run_ExistentialTestArrayMutating_IntValueBuffer4, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayOneMethodCall_ClassValueBuffer1", runFunction: run_ExistentialTestArrayOneMethodCall_ClassValueBuffer1, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayOneMethodCall_ClassValueBuffer2", runFunction: run_ExistentialTestArrayOneMethodCall_ClassValueBuffer2, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayOneMethodCall_ClassValueBuffer3", runFunction: run_ExistentialTestArrayOneMethodCall_ClassValueBuffer3, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayOneMethodCall_ClassValueBuffer4", runFunction: run_ExistentialTestArrayOneMethodCall_ClassValueBuffer4, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayOneMethodCall_IntValueBuffer0", runFunction: run_ExistentialTestArrayOneMethodCall_IntValueBuffer0, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayOneMethodCall_IntValueBuffer1", runFunction: run_ExistentialTestArrayOneMethodCall_IntValueBuffer1, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayOneMethodCall_IntValueBuffer2", runFunction: run_ExistentialTestArrayOneMethodCall_IntValueBuffer2, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayOneMethodCall_IntValueBuffer3", runFunction: run_ExistentialTestArrayOneMethodCall_IntValueBuffer3, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayOneMethodCall_IntValueBuffer4", runFunction: run_ExistentialTestArrayOneMethodCall_IntValueBuffer4, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayShift_ClassValueBuffer1", runFunction: run_ExistentialTestArrayShift_ClassValueBuffer1, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayShift_ClassValueBuffer2", runFunction: run_ExistentialTestArrayShift_ClassValueBuffer2, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayShift_ClassValueBuffer3", runFunction: run_ExistentialTestArrayShift_ClassValueBuffer3, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayShift_ClassValueBuffer4", runFunction: run_ExistentialTestArrayShift_ClassValueBuffer4, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayShift_IntValueBuffer0", runFunction: run_ExistentialTestArrayShift_IntValueBuffer0, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayShift_IntValueBuffer1", runFunction: run_ExistentialTestArrayShift_IntValueBuffer1, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayShift_IntValueBuffer2", runFunction: run_ExistentialTestArrayShift_IntValueBuffer2, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayShift_IntValueBuffer3", runFunction: run_ExistentialTestArrayShift_IntValueBuffer3, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayShift_IntValueBuffer4", runFunction: run_ExistentialTestArrayShift_IntValueBuffer4, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayTwoMethodCalls_ClassValueBuffer1", runFunction: run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer1, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayTwoMethodCalls_ClassValueBuffer2", runFunction: run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer2, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayTwoMethodCalls_ClassValueBuffer3", runFunction: run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer3, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayTwoMethodCalls_ClassValueBuffer4", runFunction: run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer4, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayTwoMethodCalls_IntValueBuffer0", runFunction: run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer0, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayTwoMethodCalls_IntValueBuffer1", runFunction: run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer1, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayTwoMethodCalls_IntValueBuffer2", runFunction: run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer2, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayTwoMethodCalls_IntValueBuffer3", runFunction: run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer3, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestArrayTwoMethodCalls_IntValueBuffer4", runFunction: run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer4, tags: [.unstable, .api, .Array]),
  BenchmarkInfo(name: "ExistentialTestMutatingAndNonMutating_ClassValueBuffer1", runFunction: run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer1, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestMutatingAndNonMutating_ClassValueBuffer2", runFunction: run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer2, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestMutatingAndNonMutating_ClassValueBuffer3", runFunction: run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer3, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestMutatingAndNonMutating_ClassValueBuffer4", runFunction: run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer4, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestMutatingAndNonMutating_IntValueBuffer0", runFunction: run_ExistentialTestMutatingAndNonMutating_IntValueBuffer0, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestMutatingAndNonMutating_IntValueBuffer1", runFunction: run_ExistentialTestMutatingAndNonMutating_IntValueBuffer1, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestMutatingAndNonMutating_IntValueBuffer2", runFunction: run_ExistentialTestMutatingAndNonMutating_IntValueBuffer2, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestMutatingAndNonMutating_IntValueBuffer3", runFunction: run_ExistentialTestMutatingAndNonMutating_IntValueBuffer3, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestMutatingAndNonMutating_IntValueBuffer4", runFunction: run_ExistentialTestMutatingAndNonMutating_IntValueBuffer4, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestMutating_ClassValueBuffer1", runFunction: run_ExistentialTestMutating_ClassValueBuffer1, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestMutating_ClassValueBuffer2", runFunction: run_ExistentialTestMutating_ClassValueBuffer2, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestMutating_ClassValueBuffer3", runFunction: run_ExistentialTestMutating_ClassValueBuffer3, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestMutating_ClassValueBuffer4", runFunction: run_ExistentialTestMutating_ClassValueBuffer4, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestMutating_IntValueBuffer0", runFunction: run_ExistentialTestMutating_IntValueBuffer0, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestMutating_IntValueBuffer1", runFunction: run_ExistentialTestMutating_IntValueBuffer1, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestMutating_IntValueBuffer2", runFunction: run_ExistentialTestMutating_IntValueBuffer2, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestMutating_IntValueBuffer3", runFunction: run_ExistentialTestMutating_IntValueBuffer3, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestMutating_IntValueBuffer4", runFunction: run_ExistentialTestMutating_IntValueBuffer4, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestOneMethodCall_ClassValueBuffer1", runFunction: run_ExistentialTestOneMethodCall_ClassValueBuffer1, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestOneMethodCall_ClassValueBuffer2", runFunction: run_ExistentialTestOneMethodCall_ClassValueBuffer2, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestOneMethodCall_ClassValueBuffer3", runFunction: run_ExistentialTestOneMethodCall_ClassValueBuffer3, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestOneMethodCall_ClassValueBuffer4", runFunction: run_ExistentialTestOneMethodCall_ClassValueBuffer4, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestOneMethodCall_IntValueBuffer0", runFunction: run_ExistentialTestOneMethodCall_IntValueBuffer0, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestOneMethodCall_IntValueBuffer1", runFunction: run_ExistentialTestOneMethodCall_IntValueBuffer1, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestOneMethodCall_IntValueBuffer2", runFunction: run_ExistentialTestOneMethodCall_IntValueBuffer2, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestOneMethodCall_IntValueBuffer3", runFunction: run_ExistentialTestOneMethodCall_IntValueBuffer3, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestOneMethodCall_IntValueBuffer4", runFunction: run_ExistentialTestOneMethodCall_IntValueBuffer4, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer1", runFunction: run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer1, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer2", runFunction: run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer2, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer3", runFunction: run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer3, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer4", runFunction: run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer4, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer0", runFunction: run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer0, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer1", runFunction: run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer1, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer2", runFunction: run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer2, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer3", runFunction: run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer3, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer4", runFunction: run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer4, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer1", runFunction: run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer1, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer2", runFunction: run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer2, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer3", runFunction: run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer3, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer4", runFunction: run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer4, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer0", runFunction: run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer0, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer1", runFunction: run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer1, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer2", runFunction: run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer2, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer3", runFunction: run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer3, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer4", runFunction: run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer4, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestTwoMethodCalls_ClassValueBuffer1", runFunction: run_ExistentialTestTwoMethodCalls_ClassValueBuffer1, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestTwoMethodCalls_ClassValueBuffer2", runFunction: run_ExistentialTestTwoMethodCalls_ClassValueBuffer2, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestTwoMethodCalls_ClassValueBuffer3", runFunction: run_ExistentialTestTwoMethodCalls_ClassValueBuffer3, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestTwoMethodCalls_ClassValueBuffer4", runFunction: run_ExistentialTestTwoMethodCalls_ClassValueBuffer4, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestTwoMethodCalls_IntValueBuffer0", runFunction: run_ExistentialTestTwoMethodCalls_IntValueBuffer0, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestTwoMethodCalls_IntValueBuffer1", runFunction: run_ExistentialTestTwoMethodCalls_IntValueBuffer1, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestTwoMethodCalls_IntValueBuffer2", runFunction: run_ExistentialTestTwoMethodCalls_IntValueBuffer2, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestTwoMethodCalls_IntValueBuffer3", runFunction: run_ExistentialTestTwoMethodCalls_IntValueBuffer3, tags: [.unstable]),
  BenchmarkInfo(name: "ExistentialTestTwoMethodCalls_IntValueBuffer4", runFunction: run_ExistentialTestTwoMethodCalls_IntValueBuffer4, tags: [.unstable]),
]

class ClassWithArrs {
    var N: Int = 0
    var A: [Int]
    var B: [Int]

    init(N: Int) {
        self.N = N

        A = [Int](repeating: 0, count: N)
        B = [Int](repeating: 0, count: N)
    }

    func readArr() {
        for i in 0..<self.N {
            for j in 0..<i {
				let _ = A[j]
				let _ = B[j]
			}
        }
    }

    func writeArr() {
		for i in 0..<self.N {
			A[i] = i
			B[i] = i
		}
    }
}

public func run_DistinctClassFieldAccesses(_ N: Int) {
    let workload = ClassWithArrs(N: 100)
    for _ in 1...N {
        workload.writeArr()
        workload.readArr()
    }
}

protocol Existential {
  init()
  func doIt() -> Bool
  func reallyDoIt() -> Bool
  mutating func mutateIt() -> Bool
}

struct IntValueBuffer0 : Existential {
  func doIt() -> Bool {
    return true
	}
  func reallyDoIt() -> Bool {
    return true
  }
  mutating func mutateIt() -> Bool {
    return true
	}
}

func next(_ x: inout Int, upto mod: Int) {
  x = (x + 1) % (mod + 1)
}

struct IntValueBuffer1 : Existential {
	var f0: Int = 0

  func doIt() -> Bool {
    return f0 == 0
	}
  func reallyDoIt() -> Bool {
    return true
  }
  mutating func mutateIt() -> Bool {
		next(&f0, upto: 1)
    return true
	}
}

struct IntValueBuffer2 : Existential {
	var f0: Int = 0
	var f1: Int = 3

  func doIt() -> Bool {
    return f0 == 0
	}
  func reallyDoIt() -> Bool {
   return f0 == 0 && f1 == 3
  }
  mutating func mutateIt() -> Bool {
    next(&f0, upto: 1)
    next(&f1, upto: 3)
    return true
	}
}

struct IntValueBuffer3 : Existential {
	var f0: Int = 0
	var f1: Int = 3
	var f2: Int = 7

  func doIt() -> Bool {
    return f0 == 0
	}
  func reallyDoIt() -> Bool {
   return f0 == 0 && f1 == 3 && f2 == 7
  }

  mutating func mutateIt() -> Bool{
    next(&f0, upto: 1)
    next(&f1, upto: 3)
    next(&f2, upto: 7)
    return true
	}
}

struct IntValueBuffer4 : Existential {
	var f0: Int = 0
	var f1: Int = 3
	var f2: Int = 7
	var f3: Int = 13

  func doIt() -> Bool {
    return f0 == 0
	}
  func reallyDoIt() -> Bool {
   return f0 == 0 && f1 == 3 && f2 == 7 && f3 == 13
  }

  mutating func mutateIt() -> Bool{
    next(&f0, upto: 1)
    next(&f1, upto: 3)
    next(&f2, upto: 7)
    next(&f3, upto: 13)
    return true
	}
}

class Klazz {
  var f0: Int = 0
  var f1: Int = 3

  func doIt() -> Bool {
    return f0 == 0
	}
  func reallyDoIt() -> Bool {
   return f0 == 0 && f1 == 3
  }

  func mutateIt() -> Bool{
    next(&f0, upto: 1)
    next(&f1, upto: 3)
    return true
	}
}

struct ClassValueBuffer1 : Existential {
	var f0: Klazz = Klazz()

  func doIt() -> Bool {
    return f0.doIt()
	}
  func reallyDoIt() -> Bool {
   return f0.reallyDoIt()
  }

  mutating func mutateIt() -> Bool{
    return f0.mutateIt()
  }
}

struct ClassValueBuffer2 : Existential {
	var f0: Klazz = Klazz()
	var f1: Klazz = Klazz()

  func doIt() -> Bool {
    return f0.doIt()
	}
  func reallyDoIt() -> Bool {
   return f0.reallyDoIt()
  }

  mutating func mutateIt() -> Bool{
    return f0.mutateIt()
  }
}

struct ClassValueBuffer3 : Existential {
	var f0: Klazz = Klazz()
	var f1: Klazz = Klazz()
	var f2: Klazz = Klazz()

  func doIt() -> Bool {
    return f0.doIt()
	}
  func reallyDoIt() -> Bool {
   return f0.reallyDoIt()
  }

  mutating func mutateIt() -> Bool{
    return f0.mutateIt()
  }
}

struct ClassValueBuffer4 : Existential {
	var f0: Klazz = Klazz()
	var f1: Klazz = Klazz()
	var f2: Klazz = Klazz()
  var f3: Int = 0

  func doIt() -> Bool {
    return f0.doIt()
	}

  func reallyDoIt() -> Bool {
   return f0.reallyDoIt()
  }

  mutating func mutateIt() -> Bool{
    return f0.mutateIt()
  }
}


@inline(never)
func initExistential<T: Existential>(withType: T.Type) -> Existential {
  return T()
}

@inline(never)
func initExistentialArray<T: Existential>(withType: T.Type, count c: Int) -> [Existential] {
  return [T](repeating: T(), count: c)
}

@inline(never)
func passExistentialTwiceOneMethodCall(_ e0: Existential, _ e1: Existential) -> Bool {
  return e0.doIt() && e1.doIt()
}

@inline(never)
func passExistentialTwiceTwoMethodCalls(_ e0: Existential, _ e1: Existential) -> Bool {
  return e0.doIt() && e1.doIt() && e0.reallyDoIt() && e1.reallyDoIt()
}

func runTestOneMethodCall<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  let existential = initExistential(withType: T.self)
  for _ in 0 ..< N {
    for _ in 0 ..< 5_000_000 {
      if !existential.doIt() {
        fatalError("expected true")
      }
    }
  }
}

func runTestTwoMethodCalls<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  let existential = initExistential(withType: T.self)
  for _ in 0 ..< N {
    for _ in 0 ..< 5_000_000 {
      if !existential.doIt()  || !existential.reallyDoIt() {
        fatalError("expected true")
      }
    }
  }
}

func runTestPassExistentialOneMethodCall<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  let existential = initExistential(withType: T.self)
  let existential2 = initExistential(withType: T.self)
  for _ in 0 ..< N {
    for _ in 0 ..< 5_000_000 {
      if !passExistentialTwiceOneMethodCall(existential, existential2) {
        fatalError("expected true")
      }
    }
  }
}

func runTestPassExistentialTwoMethodCalls<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  let existential = initExistential(withType: T.self)
  let existential2 = initExistential(withType: T.self)
  for _ in 0 ..< N {
    for _ in 0 ..< 5_000_000 {
      if !passExistentialTwiceTwoMethodCalls(existential, existential2) {
        fatalError("expected true")
      }
    }
  }
}

func runTestMutating<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  var existential = initExistential(withType: T.self)
  for _ in 0 ..< N {
    for _ in 0 ..< 5_000_000 {
      if !existential.mutateIt()  {
        fatalError("expected true")
      }
    }
  }
}

func runTestMutatingAndNonMutating<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  var existential = initExistential(withType: T.self)
  for _ in 0 ..< N {
    for _ in 0 ..< 5_000_000 {
      let _ = existential.doIt()
      if !existential.mutateIt()  {
        fatalError("expected true")
      }
    }
  }
}

func runTestArrayOneMethodCall<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  let existentialArray = initExistentialArray(withType: T.self, count: 128)
  for _ in 0 ..< N {
    for _ in 0 ..< 5_000 {
      for elt in existentialArray {
        if !elt.doIt()  {
          fatalError("expected true")
        }
      }
    }
  }
}

func runTestArrayTwoMethodCalls<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  let existentialArray = initExistentialArray(withType: T.self, count: 128)
  for _ in 0 ..< N {
    for _ in 0 ..< 5_000 {
      for elt in existentialArray {
        if !elt.doIt() || !elt.reallyDoIt() {
          fatalError("expected true")
        }
      }
    }
  }
}

func runTestArrayMutating<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  var existentialArray = initExistentialArray(withType: T.self, count: 128)
  for _ in 0 ..< N {
    for _ in 0 ..< 5_000 {
      for i in 0 ..< existentialArray.count {
        if !existentialArray[i].mutateIt()  {
          fatalError("expected true")
        }
      }
    }
  }
}

func runTestArrayShift<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  var existentialArray = initExistentialArray(withType: T.self, count: 128)
  for _ in 0 ..< N {
    for _ in 0 ..< 5_000 {
      for i in 0 ..< existentialArray.count-1 {
        existentialArray.swapAt(i, i+1)
      }
    }
  }
}
func runTestArrayConditionalShift<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  var existentialArray = initExistentialArray(withType: T.self, count: 128)
  for _ in 0 ..< N {
    for _ in 0 ..< 5_000 {
      for i in 0 ..< existentialArray.count-1 {
        let curr = existentialArray[i]
        if curr.doIt() {
          existentialArray[i] = existentialArray[i+1]
          existentialArray[i+1] = curr
        }
      }
    }
  }
}

// TestOneMethodCall.
public func run_ExistentialTestOneMethodCall_IntValueBuffer0(_ N: Int) {
  runTestOneMethodCall(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func run_ExistentialTestOneMethodCall_IntValueBuffer1(_ N: Int) {
  runTestOneMethodCall(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestOneMethodCall_IntValueBuffer2(_ N: Int) {
  runTestOneMethodCall(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestOneMethodCall_IntValueBuffer3(_ N: Int) {
  runTestOneMethodCall(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestOneMethodCall_IntValueBuffer4(_ N: Int) {
  runTestOneMethodCall(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func run_ExistentialTestOneMethodCall_ClassValueBuffer1(_ N: Int) {
  runTestOneMethodCall(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestOneMethodCall_ClassValueBuffer2(_ N: Int) {
  runTestOneMethodCall(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestOneMethodCall_ClassValueBuffer3(_ N: Int) {
  runTestOneMethodCall(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestOneMethodCall_ClassValueBuffer4(_ N: Int) {
  runTestOneMethodCall(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestTwoMethodCalls.
public func run_ExistentialTestTwoMethodCalls_IntValueBuffer0(_ N: Int) {
  runTestTwoMethodCalls(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func run_ExistentialTestTwoMethodCalls_IntValueBuffer1(_ N: Int) {
  runTestTwoMethodCalls(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestTwoMethodCalls_IntValueBuffer2(_ N: Int) {
  runTestTwoMethodCalls(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestTwoMethodCalls_IntValueBuffer3(_ N: Int) {
  runTestTwoMethodCalls(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestTwoMethodCalls_IntValueBuffer4(_ N: Int) {
  runTestTwoMethodCalls(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func run_ExistentialTestTwoMethodCalls_ClassValueBuffer1(_ N: Int) {
  runTestTwoMethodCalls(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestTwoMethodCalls_ClassValueBuffer2(_ N: Int) {
  runTestTwoMethodCalls(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestTwoMethodCalls_ClassValueBuffer3(_ N: Int) {
  runTestTwoMethodCalls(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestTwoMethodCalls_ClassValueBuffer4(_ N: Int) {
  runTestTwoMethodCalls(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestPassExistentialOneMethodCall.
public func run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer0(_ N: Int) {
  runTestPassExistentialOneMethodCall(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer1(_ N: Int) {
  runTestPassExistentialOneMethodCall(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer2(_ N: Int) {
  runTestPassExistentialOneMethodCall(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer3(_ N: Int) {
  runTestPassExistentialOneMethodCall(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer4(_ N: Int) {
  runTestPassExistentialOneMethodCall(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer1(_ N: Int) {
  runTestPassExistentialOneMethodCall(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer2(_ N: Int) {
  runTestPassExistentialOneMethodCall(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer3(_ N: Int) {
  runTestPassExistentialOneMethodCall(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer4(_ N: Int) {
  runTestPassExistentialOneMethodCall(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestPassExistentialTwoMethodCalls.
public func run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer0(_ N: Int) {
  runTestPassExistentialTwoMethodCalls(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer1(_ N: Int) {
  runTestPassExistentialTwoMethodCalls(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer2(_ N: Int) {
  runTestPassExistentialTwoMethodCalls(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer3(_ N: Int) {
  runTestPassExistentialTwoMethodCalls(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer4(_ N: Int) {
  runTestPassExistentialTwoMethodCalls(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer1(_ N: Int) {
  runTestPassExistentialTwoMethodCalls(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer2(_ N: Int) {
  runTestPassExistentialTwoMethodCalls(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer3(_ N: Int) {
  runTestPassExistentialTwoMethodCalls(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer4(_ N: Int) {
  runTestPassExistentialTwoMethodCalls(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestMutating.
public func run_ExistentialTestMutating_IntValueBuffer0(_ N: Int) {
  runTestMutating(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func run_ExistentialTestMutating_IntValueBuffer1(_ N: Int) {
  runTestMutating(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestMutating_IntValueBuffer2(_ N: Int) {
  runTestMutating(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestMutating_IntValueBuffer3(_ N: Int) {
  runTestMutating(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestMutating_IntValueBuffer4(_ N: Int) {
  runTestMutating(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func run_ExistentialTestMutating_ClassValueBuffer1(_ N: Int) {
  runTestMutating(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestMutating_ClassValueBuffer2(_ N: Int) {
  runTestMutating(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestMutating_ClassValueBuffer3(_ N: Int) {
  runTestMutating(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestMutating_ClassValueBuffer4(_ N: Int) {
  runTestMutating(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestMutatingAndNonMutating.
public func run_ExistentialTestMutatingAndNonMutating_IntValueBuffer0(_ N: Int) {
  runTestMutatingAndNonMutating(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func run_ExistentialTestMutatingAndNonMutating_IntValueBuffer1(_ N: Int) {
  runTestMutatingAndNonMutating(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestMutatingAndNonMutating_IntValueBuffer2(_ N: Int) {
  runTestMutatingAndNonMutating(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestMutatingAndNonMutating_IntValueBuffer3(_ N: Int) {
  runTestMutatingAndNonMutating(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestMutatingAndNonMutating_IntValueBuffer4(_ N: Int) {
  runTestMutatingAndNonMutating(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer1(_ N: Int) {
  runTestMutatingAndNonMutating(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer2(_ N: Int) {
  runTestMutatingAndNonMutating(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer3(_ N: Int) {
  runTestMutatingAndNonMutating(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer4(_ N: Int) {
  runTestMutatingAndNonMutating(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestArrayOneMethodCall.
public func run_ExistentialTestArrayOneMethodCall_IntValueBuffer0(_ N: Int) {
  runTestArrayOneMethodCall(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayOneMethodCall_IntValueBuffer1(_ N: Int) {
  runTestArrayOneMethodCall(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayOneMethodCall_IntValueBuffer2(_ N: Int) {
  runTestArrayOneMethodCall(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayOneMethodCall_IntValueBuffer3(_ N: Int) {
  runTestArrayOneMethodCall(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayOneMethodCall_IntValueBuffer4(_ N: Int) {
  runTestArrayOneMethodCall(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayOneMethodCall_ClassValueBuffer1(_ N: Int) {
  runTestArrayOneMethodCall(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayOneMethodCall_ClassValueBuffer2(_ N: Int) {
  runTestArrayOneMethodCall(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayOneMethodCall_ClassValueBuffer3(_ N: Int) {
  runTestArrayOneMethodCall(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayOneMethodCall_ClassValueBuffer4(_ N: Int) {
  runTestArrayOneMethodCall(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestArrayTwoMethodCalls.
public func run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer0(_ N: Int) {
  runTestArrayTwoMethodCalls(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer1(_ N: Int) {
  runTestArrayTwoMethodCalls(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer2(_ N: Int) {
  runTestArrayTwoMethodCalls(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer3(_ N: Int) {
  runTestArrayTwoMethodCalls(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer4(_ N: Int) {
  runTestArrayTwoMethodCalls(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer1(_ N: Int) {
  runTestArrayTwoMethodCalls(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer2(_ N: Int) {
  runTestArrayTwoMethodCalls(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer3(_ N: Int) {
  runTestArrayTwoMethodCalls(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer4(_ N: Int) {
  runTestArrayTwoMethodCalls(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestArrayMutating.
public func run_ExistentialTestArrayMutating_IntValueBuffer0(_ N: Int) {
  runTestArrayMutating(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayMutating_IntValueBuffer1(_ N: Int) {
  runTestArrayMutating(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayMutating_IntValueBuffer2(_ N: Int) {
  runTestArrayMutating(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayMutating_IntValueBuffer3(_ N: Int) {
  runTestArrayMutating(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayMutating_IntValueBuffer4(_ N: Int) {
  runTestArrayMutating(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayMutating_ClassValueBuffer1(_ N: Int) {
  runTestArrayMutating(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayMutating_ClassValueBuffer2(_ N: Int) {
  runTestArrayMutating(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayMutating_ClassValueBuffer3(_ N: Int) {
  runTestArrayMutating(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayMutating_ClassValueBuffer4(_ N: Int) {
  runTestArrayMutating(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestArrayShift.
public func run_ExistentialTestArrayShift_IntValueBuffer0(_ N: Int) {
  runTestArrayShift(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayShift_IntValueBuffer1(_ N: Int) {
  runTestArrayShift(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayShift_IntValueBuffer2(_ N: Int) {
  runTestArrayShift(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayShift_IntValueBuffer3(_ N: Int) {
  runTestArrayShift(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayShift_IntValueBuffer4(_ N: Int) {
  runTestArrayShift(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayShift_ClassValueBuffer1(_ N: Int) {
  runTestArrayShift(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayShift_ClassValueBuffer2(_ N: Int) {
  runTestArrayShift(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayShift_ClassValueBuffer3(_ N: Int) {
  runTestArrayShift(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayShift_ClassValueBuffer4(_ N: Int) {
  runTestArrayShift(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestArrayConditionalShift.
public func run_ExistentialTestArrayConditionalShift_IntValueBuffer0(_ N: Int) {
  runTestArrayConditionalShift(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayConditionalShift_IntValueBuffer1(_ N: Int) {
  runTestArrayConditionalShift(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayConditionalShift_IntValueBuffer2(_ N: Int) {
  runTestArrayConditionalShift(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayConditionalShift_IntValueBuffer3(_ N: Int) {
  runTestArrayConditionalShift(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayConditionalShift_IntValueBuffer4(_ N: Int) {
  runTestArrayConditionalShift(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayConditionalShift_ClassValueBuffer1(_ N: Int) {
  runTestArrayConditionalShift(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayConditionalShift_ClassValueBuffer2(_ N: Int) {
  runTestArrayConditionalShift(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayConditionalShift_ClassValueBuffer3(_ N: Int) {
  runTestArrayConditionalShift(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func run_ExistentialTestArrayConditionalShift_ClassValueBuffer4(_ N: Int) {
  runTestArrayConditionalShift(withType: ClassValueBuffer4.self, numberOfTimes: N)
}
