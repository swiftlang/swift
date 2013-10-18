// RUN: %swift %s -o /dev/null -verify

func ifFalse() -> Int {
  if false {
    return 0 // expected-warning {{will never be executed}}
  } else {
    return 1  
  }
}

func ifTrue() -> Int {
  var x = 0
  if true {
    return 1
  }
  return 0 // expected-warning {{will never be executed}}
}


func whileTrue() {
  var x = 0
  while true {
    x++
  }
  x-- // expected-warning {{will never be executed}}
}

func whileTrueReachable(v: Int) -> () {
  var x = 0
  while true {
    if v == 0 {
      break
    }
    x++
  }
  x--  
}

func whileTrueTwoPredecessorsEliminated() -> () {
    var x = 0
    var v = 0
    while (true) {
      if false {
        break
      }
      x++
    }
    x--  // expected-warning {{will never be executed}}
}

func unreachableBranch() -> Int {
  if false {
    if true { // expected-warning {{will never be executed}}
      return 0
    } 
  } else {
    return 1  
  }
}

// We should not report unreachable user code inside inlined transparent function.
@transparent
func ifTrueTransparent(b : Bool) -> Int {
  var x = 0
  if b {
    return 1
  }
  return 0
}
func testIfTrueTransparent() {
  ifTrueTransparent(true)  // no-warning
  ifTrueTransparent(false)  // no-warning
}

// We should not report unreachable user code inside generic instantiations.
// TODO: This test should start failing after we add support for generic 
// specialization in SIL. To fix it, add generic instantiation detection 
// within the DeadCodeElimination pass to address the corresponding FIXME note.
protocol HavingGetCond {
  func getCond() -> Bool
}
struct ReturnsTrue : HavingGetCond {
  func getCond() -> Bool { return true }
}
struct ReturnsOpaque : HavingGetCond {
  var b: Bool
  func getCond() -> Bool { return b }
}
func ifTrueGeneric<T : HavingGetCond>(x: T) -> Int {
  if x.getCond() {
    return 1
  }
  return 0
}
func testIfTrueGeneric(b1 : ReturnsOpaque, b2 : ReturnsTrue) {
  ifTrueGeneric(b1)  // no-warning
  ifTrueGeneric(b2)  // no-warning
}
