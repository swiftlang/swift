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
        swap(&existentialArray[i], &existentialArray[i+1])
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
public func runTestOneMethodCall_IntValueBuffer0(_ N: Int) {
  runTestOneMethodCall(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func runTestOneMethodCall_IntValueBuffer1(_ N: Int) {
  runTestOneMethodCall(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func runTestOneMethodCall_IntValueBuffer2(_ N: Int) {
  runTestOneMethodCall(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func runTestOneMethodCall_IntValueBuffer3(_ N: Int) {
  runTestOneMethodCall(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func runTestOneMethodCall_IntValueBuffer4(_ N: Int) {
  runTestOneMethodCall(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func runTestOneMethodCall_ClassValueBuffer1(_ N: Int) {
  runTestOneMethodCall(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func runTestOneMethodCall_ClassValueBuffer2(_ N: Int) {
  runTestOneMethodCall(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func runTestOneMethodCall_ClassValueBuffer3(_ N: Int) {
  runTestOneMethodCall(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func runTestOneMethodCall_ClassValueBuffer4(_ N: Int) {
  runTestOneMethodCall(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestTwoMethodCalls.
public func runTestTwoMethodCalls_IntValueBuffer0(_ N: Int) {
  runTestTwoMethodCalls(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func runTestTwoMethodCalls_IntValueBuffer1(_ N: Int) {
  runTestTwoMethodCalls(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func runTestTwoMethodCalls_IntValueBuffer2(_ N: Int) {
  runTestTwoMethodCalls(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func runTestTwoMethodCalls_IntValueBuffer3(_ N: Int) {
  runTestTwoMethodCalls(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func runTestTwoMethodCalls_IntValueBuffer4(_ N: Int) {
  runTestTwoMethodCalls(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func runTestTwoMethodCalls_ClassValueBuffer1(_ N: Int) {
  runTestTwoMethodCalls(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func runTestTwoMethodCalls_ClassValueBuffer2(_ N: Int) {
  runTestTwoMethodCalls(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func runTestTwoMethodCalls_ClassValueBuffer3(_ N: Int) {
  runTestTwoMethodCalls(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func runTestTwoMethodCalls_ClassValueBuffer4(_ N: Int) {
  runTestTwoMethodCalls(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestPassExistentialOneMethodCall.
public func runTestPassExistentialOneMethodCall_IntValueBuffer0(_ N: Int) {
  runTestPassExistentialOneMethodCall(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func runTestPassExistentialOneMethodCall_IntValueBuffer1(_ N: Int) {
  runTestPassExistentialOneMethodCall(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func runTestPassExistentialOneMethodCall_IntValueBuffer2(_ N: Int) {
  runTestPassExistentialOneMethodCall(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func runTestPassExistentialOneMethodCall_IntValueBuffer3(_ N: Int) {
  runTestPassExistentialOneMethodCall(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func runTestPassExistentialOneMethodCall_IntValueBuffer4(_ N: Int) {
  runTestPassExistentialOneMethodCall(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func runTestPassExistentialOneMethodCall_ClassValueBuffer1(_ N: Int) {
  runTestPassExistentialOneMethodCall(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func runTestPassExistentialOneMethodCall_ClassValueBuffer2(_ N: Int) {
  runTestPassExistentialOneMethodCall(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func runTestPassExistentialOneMethodCall_ClassValueBuffer3(_ N: Int) {
  runTestPassExistentialOneMethodCall(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func runTestPassExistentialOneMethodCall_ClassValueBuffer4(_ N: Int) {
  runTestPassExistentialOneMethodCall(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestPassExistentialTwoMethodCalls.
public func runTestPassExistentialTwoMethodCalls_IntValueBuffer0(_ N: Int) {
  runTestPassExistentialTwoMethodCalls(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func runTestPassExistentialTwoMethodCalls_IntValueBuffer1(_ N: Int) {
  runTestPassExistentialTwoMethodCalls(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func runTestPassExistentialTwoMethodCalls_IntValueBuffer2(_ N: Int) {
  runTestPassExistentialTwoMethodCalls(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func runTestPassExistentialTwoMethodCalls_IntValueBuffer3(_ N: Int) {
  runTestPassExistentialTwoMethodCalls(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func runTestPassExistentialTwoMethodCalls_IntValueBuffer4(_ N: Int) {
  runTestPassExistentialTwoMethodCalls(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func runTestPassExistentialTwoMethodCalls_ClassValueBuffer1(_ N: Int) {
  runTestPassExistentialTwoMethodCalls(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func runTestPassExistentialTwoMethodCalls_ClassValueBuffer2(_ N: Int) {
  runTestPassExistentialTwoMethodCalls(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func runTestPassExistentialTwoMethodCalls_ClassValueBuffer3(_ N: Int) {
  runTestPassExistentialTwoMethodCalls(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func runTestPassExistentialTwoMethodCalls_ClassValueBuffer4(_ N: Int) {
  runTestPassExistentialTwoMethodCalls(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestMutating.
public func runTestMutating_IntValueBuffer0(_ N: Int) {
  runTestMutating(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func runTestMutating_IntValueBuffer1(_ N: Int) {
  runTestMutating(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func runTestMutating_IntValueBuffer2(_ N: Int) {
  runTestMutating(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func runTestMutating_IntValueBuffer3(_ N: Int) {
  runTestMutating(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func runTestMutating_IntValueBuffer4(_ N: Int) {
  runTestMutating(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func runTestMutating_ClassValueBuffer1(_ N: Int) {
  runTestMutating(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func runTestMutating_ClassValueBuffer2(_ N: Int) {
  runTestMutating(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func runTestMutating_ClassValueBuffer3(_ N: Int) {
  runTestMutating(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func runTestMutating_ClassValueBuffer4(_ N: Int) {
  runTestMutating(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestMutatingAndNonMutating.
public func runTestMutatingAndNonMutating_IntValueBuffer0(_ N: Int) {
  runTestMutatingAndNonMutating(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func runTestMutatingAndNonMutating_IntValueBuffer1(_ N: Int) {
  runTestMutatingAndNonMutating(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func runTestMutatingAndNonMutating_IntValueBuffer2(_ N: Int) {
  runTestMutatingAndNonMutating(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func runTestMutatingAndNonMutating_IntValueBuffer3(_ N: Int) {
  runTestMutatingAndNonMutating(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func runTestMutatingAndNonMutating_IntValueBuffer4(_ N: Int) {
  runTestMutatingAndNonMutating(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func runTestMutatingAndNonMutating_ClassValueBuffer1(_ N: Int) {
  runTestMutatingAndNonMutating(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func runTestMutatingAndNonMutating_ClassValueBuffer2(_ N: Int) {
  runTestMutatingAndNonMutating(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func runTestMutatingAndNonMutating_ClassValueBuffer3(_ N: Int) {
  runTestMutatingAndNonMutating(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func runTestMutatingAndNonMutating_ClassValueBuffer4(_ N: Int) {
  runTestMutatingAndNonMutating(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestArrayOneMethodCall.
public func runTestArrayOneMethodCall_IntValueBuffer0(_ N: Int) {
  runTestArrayOneMethodCall(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func runTestArrayOneMethodCall_IntValueBuffer1(_ N: Int) {
  runTestArrayOneMethodCall(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func runTestArrayOneMethodCall_IntValueBuffer2(_ N: Int) {
  runTestArrayOneMethodCall(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func runTestArrayOneMethodCall_IntValueBuffer3(_ N: Int) {
  runTestArrayOneMethodCall(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func runTestArrayOneMethodCall_IntValueBuffer4(_ N: Int) {
  runTestArrayOneMethodCall(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func runTestArrayOneMethodCall_ClassValueBuffer1(_ N: Int) {
  runTestArrayOneMethodCall(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func runTestArrayOneMethodCall_ClassValueBuffer2(_ N: Int) {
  runTestArrayOneMethodCall(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func runTestArrayOneMethodCall_ClassValueBuffer3(_ N: Int) {
  runTestArrayOneMethodCall(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func runTestArrayOneMethodCall_ClassValueBuffer4(_ N: Int) {
  runTestArrayOneMethodCall(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestArrayTwoMethodCalls.
public func runTestArrayTwoMethodCalls_IntValueBuffer0(_ N: Int) {
  runTestArrayTwoMethodCalls(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func runTestArrayTwoMethodCalls_IntValueBuffer1(_ N: Int) {
  runTestArrayTwoMethodCalls(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func runTestArrayTwoMethodCalls_IntValueBuffer2(_ N: Int) {
  runTestArrayTwoMethodCalls(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func runTestArrayTwoMethodCalls_IntValueBuffer3(_ N: Int) {
  runTestArrayTwoMethodCalls(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func runTestArrayTwoMethodCalls_IntValueBuffer4(_ N: Int) {
  runTestArrayTwoMethodCalls(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func runTestArrayTwoMethodCalls_ClassValueBuffer1(_ N: Int) {
  runTestArrayTwoMethodCalls(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func runTestArrayTwoMethodCalls_ClassValueBuffer2(_ N: Int) {
  runTestArrayTwoMethodCalls(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func runTestArrayTwoMethodCalls_ClassValueBuffer3(_ N: Int) {
  runTestArrayTwoMethodCalls(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func runTestArrayTwoMethodCalls_ClassValueBuffer4(_ N: Int) {
  runTestArrayTwoMethodCalls(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestArrayMutating.
public func runTestArrayMutating_IntValueBuffer0(_ N: Int) {
  runTestArrayMutating(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func runTestArrayMutating_IntValueBuffer1(_ N: Int) {
  runTestArrayMutating(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func runTestArrayMutating_IntValueBuffer2(_ N: Int) {
  runTestArrayMutating(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func runTestArrayMutating_IntValueBuffer3(_ N: Int) {
  runTestArrayMutating(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func runTestArrayMutating_IntValueBuffer4(_ N: Int) {
  runTestArrayMutating(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func runTestArrayMutating_ClassValueBuffer1(_ N: Int) {
  runTestArrayMutating(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func runTestArrayMutating_ClassValueBuffer2(_ N: Int) {
  runTestArrayMutating(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func runTestArrayMutating_ClassValueBuffer3(_ N: Int) {
  runTestArrayMutating(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func runTestArrayMutating_ClassValueBuffer4(_ N: Int) {
  runTestArrayMutating(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestArrayShift.
public func runTestArrayShift_IntValueBuffer0(_ N: Int) {
  runTestArrayShift(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func runTestArrayShift_IntValueBuffer1(_ N: Int) {
  runTestArrayShift(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func runTestArrayShift_IntValueBuffer2(_ N: Int) {
  runTestArrayShift(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func runTestArrayShift_IntValueBuffer3(_ N: Int) {
  runTestArrayShift(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func runTestArrayShift_IntValueBuffer4(_ N: Int) {
  runTestArrayShift(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func runTestArrayShift_ClassValueBuffer1(_ N: Int) {
  runTestArrayShift(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func runTestArrayShift_ClassValueBuffer2(_ N: Int) {
  runTestArrayShift(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func runTestArrayShift_ClassValueBuffer3(_ N: Int) {
  runTestArrayShift(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func runTestArrayShift_ClassValueBuffer4(_ N: Int) {
  runTestArrayShift(withType: ClassValueBuffer4.self, numberOfTimes: N)
}

// TestArrayConditionalShift.
public func runTestArrayConditionalShift_IntValueBuffer0(_ N: Int) {
  runTestArrayConditionalShift(withType: IntValueBuffer0.self, numberOfTimes: N)
}
public func runTestArrayConditionalShift_IntValueBuffer1(_ N: Int) {
  runTestArrayConditionalShift(withType: IntValueBuffer1.self, numberOfTimes: N)
}
public func runTestArrayConditionalShift_IntValueBuffer2(_ N: Int) {
  runTestArrayConditionalShift(withType: IntValueBuffer2.self, numberOfTimes: N)
}
public func runTestArrayConditionalShift_IntValueBuffer3(_ N: Int) {
  runTestArrayConditionalShift(withType: IntValueBuffer3.self, numberOfTimes: N)
}
public func runTestArrayConditionalShift_IntValueBuffer4(_ N: Int) {
  runTestArrayConditionalShift(withType: IntValueBuffer4.self, numberOfTimes: N)
}
public func runTestArrayConditionalShift_ClassValueBuffer1(_ N: Int) {
  runTestArrayConditionalShift(withType: ClassValueBuffer1.self, numberOfTimes: N)
}
public func runTestArrayConditionalShift_ClassValueBuffer2(_ N: Int) {
  runTestArrayConditionalShift(withType: ClassValueBuffer2.self, numberOfTimes: N)
}
public func runTestArrayConditionalShift_ClassValueBuffer3(_ N: Int) {
  runTestArrayConditionalShift(withType: ClassValueBuffer3.self, numberOfTimes: N)
}
public func runTestArrayConditionalShift_ClassValueBuffer4(_ N: Int) {
  runTestArrayConditionalShift(withType: ClassValueBuffer4.self, numberOfTimes: N)
}
