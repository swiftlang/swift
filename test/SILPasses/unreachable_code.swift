// RUN: %swift %s -o /dev/null -verify

func ifFalse() -> Int {
  if false { // expected-note {{always evaluates to false}}
    return 0 // expected-warning {{will never be executed}}
  } else {
    return 1  
  }
}

func ifTrue() -> Int {
  var x = 0
  if true { // expected-note {{always evaluates to true}}
    return 1
  }
  return 0 // expected-warning {{will never be executed}}
}


func whileTrue() {
  var x = 0
  while true { // expected-note {{always evaluates to true}}
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
    while (true) { // expected-note {{always evaluates to true}}
      if false {
        break
      }
      x++
    }
    x--  // expected-warning {{will never be executed}}
}

func unreachableBranch() -> Int {
  if false { // expected-note {{always evaluates to false}}
    // FIXME: It'd be nice if the warning were on 'if true' instead of the 
    // body.
    if true {
      return 0 // expected-warning {{will never be executed}}
    } 
  } else {
    return 1  
  }
}

// We should not report unreachable user code inside inlined transparent function.
@transparent
func ifTrueTransparent(b: Bool) -> Int {
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
func testIfTrueGeneric(b1: ReturnsOpaque, b2: ReturnsTrue) {
  ifTrueGeneric(b1)  // no-warning
  ifTrueGeneric(b2)  // no-warning
}

// Test switch_enum folding/diagnostic.
enum X {
  case One
  case Two
  case Three
}

func testSwitchEnum(xi: Int) -> Int {
  var x = xi
  var cond: X = .Two
  switch cond { // expected-warning {{switch condition evaluates to a constant}}
  case .One:
    x++ // expected-note {{will never be executed}}
  case .Two:
    x--  
  case .Three:
    x--  
  }

  switch cond { // no warning
  default:
    x++
  }

  switch cond { // no warning
  case .Two: 
    x++
  }

  switch cond {
  case .One:
    x++
  } // expected-error{{switch must be exhaustive}}

  switch cond {
  case .One:
    x++
  case .Three:
    x++
  } // expected-error{{switch must be exhaustive}}

  switch cond { // expected-warning{{switch condition evaluates to a constant}}
  case .Two: 
    x++
  default: 
    x-- // expected-note{{will never be executed}}
  }

  switch cond { // expected-warning{{switch condition evaluates to a constant}}
  case .One: 
    x++ // expected-note{{will never be executed}}
  default: 
    x--
  }
  
  return x;
}

@noreturn @asmname("exit") func exit() -> ()
func reachableThroughNonFoldedPredecessor(fn: @auto_closure () -> Bool = false) {
  if !_branchHint(fn(), true) {
    exit()
  }
  var x: Int = 0 // no warning
}

func intConstantTest() -> Int{
  var y: Int = 1
  if y == 1 { // expected-note {{condition always evaluates to true}}
    return y
  }
  
  return 1 // expected-warning {{will never be executed}}
}

func intConstantTest2() -> Int{
  var y:Int = 1
  var x:Int = y 

  if x != 1 { // expected-note {{condition always evaluates to false}}
    return y // expected-warning {{will never be executed}}
  }
  return 3
}

