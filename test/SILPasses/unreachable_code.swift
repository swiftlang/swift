// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify
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

// Work-around <rdar://problem/17687851> by ensuring there is
// something that appears to be user code in unreachable blocks.
func userCode() {}

func whileTrue() {
  var x = 0
  while true { // expected-note {{always evaluates to true}}
    x++
  }
  userCode() // expected-warning {{will never be executed}}
}

func whileTrueSilent() {
  while true {
  }
}   // no warning!

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
  userCode()  // expected-warning {{will never be executed}}
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
    userCode() // expected-note {{will never be executed}}
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
    userCode() // expected-note{{will never be executed}}
  }

  switch cond { // expected-warning{{switch condition evaluates to a constant}}
  case .One: 
    userCode() // expected-note{{will never be executed}}
  default: 
    x--
  }
  
  return x;
}


// Treat nil as .None and do not emit false 
// non-exhaustive warning.
func testSwitchEnumOptionalNil(x: Int?) -> Int {
  switch x { // no warning
  case .Some(_):
    return 1
  case nil:
    return -1
  }
}

// Do not emit false non-exhaustive warnings if both
// true and false are covered by the switch.
func testSwitchEnumBool(b: Bool, xi: Int) -> Int {
  var x = xi
  var Cond = b
  
  switch Cond { // no warning
  default:
    x++
  }

  switch Cond {
  case true:
    x++
  } // expected-error{{switch must be exhaustive}}

  switch Cond {
  case false:
    x++
  } // expected-error{{switch must be exhaustive}}

  switch Cond { // no warning
  case true:
    x++
  case false:
    x--
  }

  return x
}

// Do not emit false non-exhaustive warnings if both 
// true and false are covered for a boolean element of a tuple.
func testSwitchEnumBoolTuple(b1: Bool, b2: Bool, xi: Int) -> Int {
  var x = xi
  var Cond = (b1, b2)
  
  switch Cond { // no warning
  default:
    x++
  }

  switch Cond {
  case (true, true):
    x++
    // FIXME: Two expect statements are written, because unreachable diagnostics produces N errors
    // for non-exhaustive switches on tuples of N elements
  } // expected-error{{switch must be exhaustive}} expected-error{{switch must be exhaustive}}

  switch Cond {
  case (false, true):
    x++
    // FIXME: Two expect statements are written, because unreachable diagnostics produces N errors
    // for non-exhaustive switches on tuples of N elements
  } // expected-error{{switch must be exhaustive}} expected-error{{switch must be exhaustive}}

  switch Cond { // no warning
  case (true, true):
    x++
  case (true, false):
    x++
  case (false, true):
    x--
  case (false, false):
    x--
  }

  return x
}


@noreturn @asmname("exit") func exit() -> ()
func reachableThroughNonFoldedPredecessor(@autoclosure fn: () -> Bool = false) {
  if !_fastPath(fn()) {
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

func test_single_statement_closure(fn:() -> ()) {}
test_single_statement_closure() {
    exit() // no-warning
}

class C { }
class Super { 
  var s = C()
  deinit { // no-warning
  }
}
class D : Super { 
  var c = C()
  deinit { // no-warning
    exit()
  }
}



// <rdar://problem/20097963> incorrect DI diagnostic in unreachable code
enum r20097963Test {
  case A
  case B
}

class r20097963MyClass {
  func testStr(t: r20097963Test) {
    let str: String
    switch t {
    case .A:
      str = "A"
    case .B:
      str = "B"
    default:    // expected-warning {{default will never be executed}}
      str = "unknown"  // Should not be rejected.
    }
  }
}

@noreturn
func die() { die() }

func testLetElseExprPattern(a : Int) {
  let 4 = a else {  }  // expected-error {{else block of let/else may not fall through}}
  var 4 = a else {  }  // expected-error {{else block of var/else may not fall through}}
  
  var 4 = a else { return }  // ok
  var 4 = a else { die() }  // ok
  var 4 = a else { fatalError("baaad") }  // ok

  for i in 0...100 {
    var 4 = a else { continue } // ok
  }
}

func testUnless(a : Int) {
  unless case 4 = a {  }  // expected-error {{unless body may not fall through}}

  unless case 4 = a { return }  // ok
  unless case 4 = a { die() }  // ok
  unless case 4 = a { fatalError("baaad") }  // ok

  for i in 0...100 {
    unless case 4 = a { continue } // ok
  }
}

public func testFailingCast(s:String) -> Int {
   // There should be no notes or warnings about a call to a noreturn function, because we do not expose
   // how casts are lowered.
   return s as! Int // expected-warning {{cast from 'String' to unrelated type 'Int' always fails}}
}


while true {
}
 // no warning!
