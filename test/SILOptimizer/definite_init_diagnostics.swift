// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-sil %s -parse-stdlib -o /dev/null -verify

import Swift

func markUsed<T>(_ t: T) {}

// These are tests for definite initialization, which is implemented by the
// memory promotion pass.

func test1() -> Int {
  // expected-warning @+1 {{variable 'a' was never mutated; consider changing to 'let' constant}} {{3-6=let}}
  var a : Int  // expected-note {{variable defined here}}
  return a     // expected-error {{variable 'a' used before being initialized}}
}

func takes_inout(_ a: inout Int) {}
func takes_inout_any(_ a: inout Any) {}
func takes_closure(_ fn: () -> ()) {}

class SomeClass { 
  var x : Int   // expected-note {{'self.x' not initialized}}
  
  var computedProperty : Int { return 42 }

  init() { x = 0 }
  init(b : Bool) {
    if (b) {}
  } // expected-error {{return from initializer without initializing all stored properties}}
  
  func baseMethod() {}
}

struct SomeStruct { var x = 1 }

func test2() {
  // inout.

  var a1 : Int        // expected-note {{variable defined here}}
  takes_inout(&a1)    // expected-error {{variable 'a1' passed by reference before being initialized}}

  var a2 = 4
  takes_inout(&a2)    // ok.

  var a3 : Int
  a3 = 4
  takes_inout(&a3)    // ok.
  
  // Address-of with Builtin.addressof.
  var a4 : Int            // expected-note {{variable defined here}}
  Builtin.addressof(&a4)  // expected-error {{address of variable 'a4' taken before it is initialized}}
  // expected-warning @-1 {{result of call to 'addressof' is unused}}


  // Closures.

  // expected-warning @+1 {{variable 'b1' was never mutated}} {{3-6=let}}
  var b1 : Int        // expected-note {{variable defined here}}
  takes_closure {     // expected-error {{variable 'b1' captured by a closure before being initialized}}
    markUsed(b1)
  }

  var b1a : Int        // expected-note {{variable defined here}}
  takes_closure {     // expected-error {{variable 'b1a' captured by a closure before being initialized}}
    b1a += 1
    markUsed(b1a)
  }

  var b2 = 4
  takes_closure {     // ok.
    markUsed(b2)
  }
  b2 = 1

  var b3 : Int
  b3 = 4
  takes_closure {     // ok.
    markUsed(b3)
  }
    
  var b4 : Int?
  takes_closure {     // ok.
    markUsed(b4!)
  }
  b4 = 7
  
  let b5: Any
  b5 = "x"   
  ({ takes_inout_any(&b5) })()   // expected-error {{immutable value 'b5' may not be passed inout}}

  // Structs
  var s1 : SomeStruct
  s1 = SomeStruct()   // ok
  _ = s1

  var s2 : SomeStruct  // expected-note {{variable defined here}}
  s2.x = 1             // expected-error {{struct 's2' must be completely initialized before a member is stored to}}


  // Classes
  // expected-warning @+1 {{variable 'c1' was never mutated}} {{3-6=let}}
  var c1 : SomeClass   // expected-note {{variable defined here}}
  markUsed(c1.x)          // expected-error {{variable 'c1' used before being initialized}}


  let c2 = SomeClass()
  markUsed(c2.x)          // ok
  
  
  // Weak
  weak var w1 : SomeClass?
  _ = w1                // ok: default-initialized

  weak var w2 = SomeClass()
  _ = w2                // ok
  
  
  // Unowned.  This is immediately crashing code (it causes a retain of a
  // released object) so it should be diagnosed with a warning someday.
  // expected-warning @+1 {{variable 'u1' was never mutated; consider changing to 'let' constant}} {{11-14=let}}
  unowned var u1 : SomeClass // expected-note {{variable defined here}}
  _ = u1                // expected-error {{variable 'u1' used before being initialized}}

  unowned let u2 = SomeClass()
  _ = u2                // ok
}



// Tuple field sensitivity.
func test4() {
  // expected-warning @+1 {{variable 't1' was never mutated; consider changing to 'let' constant}} {{3-6=let}}
  var t1 = (1, 2, 3)
  markUsed(t1.0 + t1.1 + t1.2)  // ok


  // expected-warning @+1 {{variable 't2' was never mutated; consider changing to 'let' constant}} {{3-6=let}}
  var t2 : (Int, Int, Int)   // expected-note 3 {{variable defined here}}
  markUsed(t2.0)   // expected-error {{variable 't2.0' used before being initialized}}
  markUsed(t2.1)   // expected-error {{variable 't2.1' used before being initialized}}
  markUsed(t2.2)   // expected-error {{variable 't2.2' used before being initialized}}


  var t3 : (Int, Int, Int)   // expected-note {{variable defined here}}
  t3.0 = 1; t3.2 = 42
  markUsed(t3.0)
  markUsed(t3.1)   // expected-error {{variable 't3.1' used before being initialized}}
  markUsed(t3.2)


  // Partially set, wholly read.
  var t4 : (Int, Int, Int)   // expected-note 1 {{variable defined here}}
  t4.0 = 1; t4.2 = 42
  _ = t4            // expected-error {{variable 't4.1' used before being initialized}}
  

  // Subelement sets.
  var t5 : (a : (Int, Int), b : Int)  // expected-note {{variable defined here}}
  t5.a = (1,2)
  markUsed(t5.a.0)
  markUsed(t5.a.1)
  markUsed(t5.b)       // expected-error {{variable 't5.b' used before being initialized}}
  

  var t6 : (a : (Int, Int), b : Int)  // expected-note {{variable defined here}}
  t6.b = 12; t6.a.1 = 1
  markUsed(t6.a.0)     // expected-error {{variable 't6.a.0' used before being initialized}}
  markUsed(t6.a.1)
  markUsed(t6.b)
}

func tupleinout(_ a: inout (lo: Int, hi: Int)) {
  markUsed(a.0)   // ok
  markUsed(a.1)   // ok
}

// Address only types
func test5<T>(_ x: T, y: T) {
  var a : ((T, T), T)  // expected-note {{variable defined here}}
  a.0 = (x, y)
  
  _ = a     // expected-error {{variable 'a.1' used before being initialized}}
}


struct IntFloatStruct { var a = 1, b = 2.0 }

func test6() -> Int {
  var a = IntFloatStruct()

  a.a = 1
  a.b = 2

  return a.a
}

// Control flow.
func test7(_ cond: Bool) {
  var a : Int
  
  if cond { a = 42 } else { a = 17 }
  markUsed(a)         // ok

  var b : Int      // expected-note {{variable defined here}}
  if cond { } else { b = 17 }
  markUsed(b)         // expected-error {{variable 'b' used before being initialized}}
}

protocol SomeProtocol {
  func protoMe()
}

protocol DerivedProtocol : SomeProtocol {}

func existentials(_ i: Int, dp: DerivedProtocol) {
  // expected-warning @+1 {{variable 'a' was written to, but never read}}
  var a : Any = ()
  a = i

  // expected-warning @+1 {{variable 'b' was written to, but never read}}
  var b : Any
  b = ()

  // expected-warning @+1 {{variable 'c' was never used}} {{7-8=_}}
  var c : Any   // no uses.
  
  // expected-warning @+1 {{variable 'd1' was never mutated}} {{3-6=let}}
  var d1 : Any  // expected-note {{variable defined here}}
  _ = d1   // expected-error {{variable 'd1' used before being initialized}}
  

  // expected-warning @+1 {{variable 'e' was never mutated}} {{3-6=let}}
  var e : SomeProtocol  // expected-note {{variable defined here}}
  e.protoMe()           // expected-error {{variable 'e' used before being initialized}}
  
  var f : SomeProtocol = dp  // ok, init'd by existential upcast.
  
  // expected-warning @+1 {{variable 'g' was never mutated}} {{3-6=let}}
  var g : DerivedProtocol   // expected-note {{variable defined here}}
  f = g                     // expected-error {{variable 'g' used before being initialized}}
  _ = f
}


// Tests for top level code.
var g1 : Int                 // expected-note {{variable defined here}}
var g2 : Int = 42

func testTopLevelCode() {    // expected-error {{variable 'g1' used by function definition before being initialized}}
  markUsed(g1)
  markUsed(g2)
}

var (g3,g4) : (Int,Int)          // expected-note 2 {{variable defined here}}
class DITLC_Class {
  init() {            // expected-error {{variable 'g3' used by function definition before being initialized}}
    markUsed(g3)
  }
  deinit {            // expected-error {{variable 'g4' used by function definition before being initialized}}
    markUsed(g4)
  }
}

struct EmptyStruct {}

func useEmptyStruct(_ a: EmptyStruct) {}

func emptyStructTest() {
  // <rdar://problem/20065892> Diagnostic for an uninitialized constant calls it a variable
  let a : EmptyStruct  // expected-note {{constant defined here}}
  useEmptyStruct(a)    // expected-error {{constant 'a' used before being initialized}}

  var (b,c,d) : (EmptyStruct,EmptyStruct,EmptyStruct) // expected-note 2 {{variable defined here}}
  
  c = EmptyStruct()

  useEmptyStruct(b)     // expected-error {{variable 'b' used before being initialized}}
  useEmptyStruct(c)
  useEmptyStruct(d)     // expected-error {{variable 'd' used before being initialized}}

  var (e,f) : (EmptyStruct,EmptyStruct)
  (e, f) = (EmptyStruct(),EmptyStruct())
  
  useEmptyStruct(e)
  useEmptyStruct(f)

  var g : (EmptyStruct,EmptyStruct)
  g = (EmptyStruct(),EmptyStruct())
  
  useEmptyStruct(g.0)
  useEmptyStruct(g.1)
}

func takesTuplePair(_ a : inout (SomeClass, SomeClass)) {}

// This tests cases where a store might be an init or assign based on control
// flow path reaching it.
func conditionalInitOrAssign(_ c : Bool, x : Int) {
  var t : Int  // Test trivial types.
  if c {
    t = 0
  }
  if c {
    t = x
  }
  t = 2
  _ = t
  
  // Nontrivial type
  var sc : SomeClass
  if c {
    sc = SomeClass()
  }
  sc = SomeClass()
  _ = sc
  
  // Tuple element types
  var tt : (SomeClass, SomeClass)

  if c {
    tt.0 = SomeClass()
  } else {
    tt.1 = SomeClass()
  }

  tt.0 = SomeClass()
  tt.1 = tt.0
  
  var t2 : (SomeClass, SomeClass)  // expected-note {{variable defined here}}
  t2.0 = SomeClass()
  takesTuplePair(&t2)   // expected-error {{variable 't2.1' passed by reference before being initialized}}
}

enum NotInitializedUnion {
  init() {
  }    // expected-error {{return from enum initializer method without storing to 'self'}}
  case X
  case Y
}

extension NotInitializedUnion {
  init(a : Int) {
  }   // expected-error {{return from enum initializer method without storing to 'self'}}
}

enum NotInitializedGenericUnion<T> {
  init() { 
  }    // expected-error {{return from enum initializer method without storing to 'self'}}
  case X
}


class SomeDerivedClass : SomeClass {
  var y : Int
  override init() { 
    y = 42  // ok
    super.init()
  }
  
  init(a : Bool) {
    super.init() // expected-error {{property 'self.y' not initialized at super.init call}}
  }

  init(a : Bool, b : Bool) {
    // x is a superclass member.  It cannot be used before we are initialized.
    x = 17  // expected-error {{use of 'self' in property access 'x' before super.init initializes self}}
    y = 42
    super.init()
  }
  
  init(a : Bool, b : Bool, c : Bool) {
    y = 42
    super.init()
  }

  init(a : Bool, b : Bool, c : Bool, d : Bool) {
    y = 42
    super.init()
    super.init() // expected-error {{super.init called multiple times in initializer}}
  }

  init(a : Bool, b : Bool, c : Bool, d : Bool, e : Bool) {
    super.init()  // expected-error {{property 'self.y' not initialized at super.init call}}
    super.init()  // expected-error {{super.init called multiple times in initializer}}
  }
  
  init(a : Bool, b : Bool, c : Bool, d : Bool, e : Bool, f : Bool) {
    y = 11
    if a { super.init() }
    x = 42        // expected-error {{use of 'self' in property access 'x' before super.init initializes self}}
  }               // expected-error {{super.init isn't called on all paths before returning from initializer}}
  
  func someMethod() {}
  
  init(a : Int) {
    y = 42
    super.init()
  }

  init(a : Int, b : Bool) {
    y = 42
    someMethod() // expected-error {{use of 'self' in method call 'someMethod' before super.init initializes self}}
    super.init()
  }

  init(a : Int, b : Int) {
    y = 42
    baseMethod()  // expected-error {{use of 'self' in method call 'baseMethod' before super.init initializes self}}
    super.init()
  }
  
  init(a : Int, b : Int, c : Int) {
    y = computedProperty  // expected-error {{use of 'self' in property access 'computedProperty' before super.init initializes self}}
    super.init()
  }

}

//===----------------------------------------------------------------------===//
//  Delegating initializers
//===----------------------------------------------------------------------===//

class DelegatingCtorClass {
  var ivar: EmptyStruct

  init() { ivar = EmptyStruct() }

  convenience init(x: EmptyStruct) {
    self.init()
    _ = ivar // okay: ivar has been initialized by the delegation above
  }
  
  convenience init(x: EmptyStruct, y: EmptyStruct) {
    _ = ivar       // expected-error {{use of 'self' in property access 'ivar' before self.init initializes self}}
    ivar = x       // expected-error {{use of 'self' in property access 'ivar' before self.init initializes self}}
    self.init()
  }

  convenience init(x: EmptyStruct, y: EmptyStruct, z: EmptyStruct) {
    self.init()
    self.init()    // expected-error {{self.init called multiple times in initializer}}
  }

  convenience init(x: (EmptyStruct, EmptyStruct)) {
    method()       // expected-error {{use of 'self' in method call 'method' before self.init initializes self}}
    self.init()
  }

  convenience init(c : Bool) {
    if c {
      return
    }
    self.init()
  }                // expected-error {{self.init isn't called on all paths before returning from initializer}}

  convenience init(bool: Bool) {
    doesNotReturn()
  }

  convenience init(double: Double) {
  } // expected-error{{self.init isn't called on all paths before returning from initializer}}

  func method() {}
}


struct DelegatingCtorStruct {
  var ivar : EmptyStruct

  init() { ivar = EmptyStruct() }


  init(a : Double) {
    self.init()
    _ = ivar // okay: ivar has been initialized by the delegation above
  }
  
  init(a : Int) {
    _ = ivar // expected-error {{'self' used before self.init call}}
    self.init()
  }

  init(a : Float) {
    self.init()
    self.init()    // expected-error {{self.init called multiple times in initializer}}
  }
  
  init(c : Bool) {
    if c {
      return
    }

    self.init()
  }                // expected-error {{self.init isn't called on all paths before returning from initializer}}

}


enum DelegatingCtorEnum {
  case Dinosaur, Train, Truck

  init() { self = .Train }

  init(a : Double) {
    self.init()
    _ = self // okay: self has been initialized by the delegation above
    self = .Dinosaur
  }
  
  init(a : Int) {
    _ = self // expected-error {{'self' used before self.init call}}
    self.init()
  }

  init(a : Float) {
    self.init()
    self.init()    // expected-error {{self.init called multiple times in initializer}}
  }
  
  init(c : Bool) {
     if c {
      return
    }

    self.init()
  }                // expected-error {{self.init isn't called on all paths before returning from initializer}}
}

//===----------------------------------------------------------------------===//
//  Delegating initializers vs extensions
//===----------------------------------------------------------------------===//

protocol TriviallyConstructible {
  init()
  func go(_ x: Int)
}

extension TriviallyConstructible {
  init(down: Int) {
    go(down) // expected-error {{'self' used before self.init call}}
    self.init()
  }
}

//===----------------------------------------------------------------------===//
//  Various bugs
//===----------------------------------------------------------------------===//

// rdar://16119509 - Dataflow problem where we reject valid code.
class rdar16119509_Buffer {
  init(x : Int) { }
}
class rdar16119509_Base {}
class rdar16119509_Derived : rdar16119509_Base {
  override init() {
    var capacity = 2
    while capacity < 2 {
      capacity <<= 1
    }
    buffer = rdar16119509_Buffer(x: capacity)
  }

  var buffer : rdar16119509_Buffer
}

// <rdar://problem/16797372> Bogus error: self.init called multiple times in initializer
extension Foo {
  convenience init() {
    for _ in 0..<42 {
    }
    self.init(a: 4)
  }
}

class Foo {
  init(a : Int) {}
}



func doesNotReturn() -> Never {
  while true {}
}

func doesReturn() {}

func testNoReturn1(_ b : Bool) -> Any {
  var a : Any
  if b {
    a = 42
  } else {
    doesNotReturn()
  }

  return a   // Ok, because the noreturn call path isn't viable.
}

func testNoReturn2(_ b : Bool) -> Any {
  var a : Any  // expected-note {{variable defined here}}
  if b {
    a = 42
  } else {
    doesReturn()
  }

  // Not ok, since doesReturn() doesn't kill control flow.
  return a   // expected-error {{variable 'a' used before being initialized}}
}

class PerpetualMotion {
  func start() -> Never {
    repeat {} while true
  }
  static func stop() -> Never {
    repeat {} while true
  }
}

func testNoReturn3(_ b : Bool) -> Any {
  let a : Int

  switch b {
  default:
    PerpetualMotion().start()
  }

  return a
}

func testNoReturn4(_ b : Bool) -> Any {
  let a : Int

  switch b {
  default:
    PerpetualMotion.stop()
  }

  return a
}


// <rdar://problem/16687555> [DI] New convenience initializers cannot call inherited convenience initializers
class BaseWithConvenienceInits {
 init(int: Int) {}
 convenience init() {
    self.init(int: 3)
 }
}

class DerivedUsingConvenienceInits : BaseWithConvenienceInits {
  convenience init(string: String) {
    self.init()
  }
}

// <rdar://problem/16660680> QoI: _preconditionFailure() in init method complains about super.init being called multiple times
class ClassWhoseInitDoesntReturn : BaseWithConvenienceInits {
  init() {  
    _preconditionFailure("leave me alone dude");
  }
}

// <rdar://problem/17233681> DI: Incorrectly diagnostic in delegating init with generic enum
enum r17233681Lazy<T> {
  case Thunk(() -> T)
  case Value(T)
  
  init(value: T) {
    self = .Value(value)
  }
}

extension r17233681Lazy {
  init(otherValue: T) {
    self.init(value: otherValue)
  }
}


// <rdar://problem/17556858> delegating init that delegates to @_transparent init fails
struct FortyTwo { }

extension Double {
  init(v : FortyTwo) {
    self.init(0.0)
  }
}

// <rdar://problem/17686667> If super.init is implicitly inserted, DI diagnostics have no location info
class r17686667Base {}
class r17686667Test : r17686667Base {
  var x: Int
  override init() {  // expected-error {{property 'self.x' not initialized at implicitly generated super.init call}}

  }
}

// <rdar://problem/18199087> DI doesn't catch use of super properties lexically inside super.init call
class r18199087BaseClass {
  let data: Int
  init(val: Int) {
    data = val
  }
}
class r18199087SubClassA: r18199087BaseClass {
  init() {
    super.init(val: self.data)  // expected-error {{use of 'self' in property access 'data' before super.init initializes self}}
  }
}

// <rdar://problem/18414728> QoI: DI should talk about "implicit use of self" instead of individual properties in some cases
class rdar18414728Base {
  var prop:String? { return "boo" }

  // expected-note @+1 {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
  let aaaaa:String  // expected-note 3 {{'self.aaaaa' not initialized}}

  init() {
    if let p1 = prop { // expected-error {{use of 'self' in property access 'prop' before all stored properties are initialized}}
      aaaaa = p1
    }
    aaaaa = "foo"  // expected-error {{immutable value 'self.aaaaa' may only be initialized once}}
  }

  init(a : ()) {
    method1(42)   // expected-error {{use of 'self' in method call 'method1' before all stored properties are initialized}}
    aaaaa = "foo"
  }

  init(b : ()) {
    final_method() // expected-error {{use of 'self' in method call 'final_method' before all stored properties are initialized}}
    aaaaa = "foo"
  }

  init(c : ()) {
    aaaaa = "foo"
    final_method()  // ok
  }

  func method1(_ a : Int) {}
  final func final_method() {}
}

class rdar18414728Derived : rdar18414728Base {
  var prop2:String? { return "boo" }

  // expected-note @+1 2 {{change 'let' to 'var' to make it mutable}} {{3-6=var}} {{3-6=var}}
  let aaaaa2:String

  override init() {
    if let p1 = prop2 {  // expected-error {{use of 'self' in property access 'prop2' before super.init initializes self}}
      aaaaa2 = p1
    }
    aaaaa2 = "foo"    // expected-error {{immutable value 'self.aaaaa2' may only be initialized once}}
    super.init()
  }

  override init(a : ()) {
    method2()            // expected-error {{use of 'self' in method call 'method2' before super.init initializes self}}
    aaaaa2 = "foo"
    super.init()
  }

  override init(b : ()) {
    aaaaa2 = "foo"
    method2()           // expected-error {{use of 'self' in method call 'method2' before super.init initializes self}}
    super.init()
  }

  override init(c : ()) {
    super.init()        // expected-error {{property 'self.aaaaa2' not initialized at super.init call}}
    aaaaa2 = "foo"      // expected-error {{immutable value 'self.aaaaa2' may only be initialized once}}
    method2()
  }

  func method2() {}
}

struct rdar18414728Struct {
  var computed:Int? { return 4 }

  var i : Int  // expected-note 2 {{'self.i' not initialized}}
  var j : Int  // expected-note {{'self.j' not initialized}}

  init() {
    j = 42
    if let p1 = computed { // expected-error {{'self' used before all stored properties are initialized}}
      i = p1
    }
    i = 1
  }
  init(a : ()) {
    method(42)     // expected-error {{'self' used before all stored properties are initialized}}
    i = 1
    j = 2
  }

  func method(_ a : Int) {}
}


extension Int {
  mutating func mutate() {}
  func inspect() {}
}


// <rdar://problem/19035287> let properties should only be initializable, not reassignable
struct LetProperties {
  // expected-note @+1 {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
  let arr : [Int]
  // expected-note @+1 2 {{change 'let' to 'var' to make it mutable}} {{3-6=var}} {{3-6=var}}
  let (u, v) : (Int, Int)
  // expected-note @+1 2 {{change 'let' to 'var' to make it mutable}} {{3-6=var}} {{3-6=var}}
  let w : (Int, Int)
  let x = 42
  // expected-note @+1 {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
  let y : Int
  let z : Int?  // expected-note{{'self.z' not initialized}}

  // Let properties can be initialized naturally exactly once along any given
  // path through an initializer.
  init(cond : Bool) {
    if cond {
      w.0 = 4
      (u,v) = (4,2)
      y = 71
    } else {
      y = 13
      v = 2
      u = v+1
      w.0 = 7
    }
    w.1 = 19
    z = nil
    arr = []
  }

  // Multiple initializations are an error.
  init(a : Int) {
    y = a
    y = a   // expected-error {{immutable value 'self.y' may only be initialized once}}

    u = a
    v = a
    u = a   // expected-error {{immutable value 'self.u' may only be initialized once}}
    v = a   // expected-error {{immutable value 'self.v' may only be initialized once}}

    w.0 = a
    w.1 = a

    w.0 = a  // expected-error {{immutable value 'self.w.0' may only be initialized once}}
    w.1 = a  // expected-error {{immutable value 'self.w.1' may only be initialized once}}

    arr = []
    arr = [] // expected-error {{immutable value 'self.arr' may only be initialized once}}
  }  // expected-error {{return from initializer without initializing all stored properties}}

  // inout uses of let properties are an error.
  init() {
    u = 1; v = 13; w = (1,2); y = 1 ; z = u

    var variable = 42
    swap(&u, &variable)  // expected-error {{immutable value 'self.u' may not be passed inout}}
    
    u.inspect()  // ok, non mutating.
    u.mutate()  // expected-error {{mutating method 'mutate' may not be used on immutable value 'self.u'}}
    
    arr = []
    arr += []      // expected-error {{mutating operator '+=' may not be used on immutable value 'self.arr'}}
    arr.append(4)  // expected-error {{mutating method 'append' may not be used on immutable value 'self.arr'}}
    arr[12] = 17   // expected-error {{mutating subscript 'subscript' may not be used on immutable value 'self.arr'}}
  }
}


// <rdar://problem/19215313> let properties don't work with protocol method dispatch
protocol TestMutabilityProtocol {
  func toIntMax()
  mutating func changeToIntMax()
}
 
class C<T : TestMutabilityProtocol> {
  let x : T
  let y : T
  
  init(a : T) {
    x = a; y = a
    x.toIntMax()
    y.changeToIntMax()  // expected-error {{mutating method 'changeToIntMax' may not be used on immutable value 'self.y'}}
  }
}

struct MyMutabilityImplementation : TestMutabilityProtocol {
  func toIntMax() {
  }
  
  mutating func changeToIntMax() {
  }
}


// <rdar://problem/16181314> don't require immediate initialization of 'let' values
func testLocalProperties(_ b : Int) -> Int {
  // expected-note @+1 {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
  let x : Int
  let y : Int // never assigned is ok    expected-warning {{immutable value 'y' was never used}} {{7-8=_}}

  x = b
  x = b    // expected-error {{immutable value 'x' may only be initialized once}}

  // This is ok, since it is assigned multiple times on different paths.
  let z : Int
  if true || false {
    z = b
  } else {
    z = b
  }

  _ = z
  return x
}

// Should be rejected as multiple assignment.
func testAddressOnlyProperty<T>(_ b : T) -> T {
  // expected-note @+1 {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
  let x : T
  let y : T
  let z : T   // never assigned is ok.  expected-warning {{immutable value 'z' was never used}} {{7-8=_}}
  x = b
  y = b
  x = b   // expected-error {{immutable value 'x' may only be initialized once}}

  var tmp = b
  swap(&x, &tmp)   // expected-error {{immutable value 'x' may not be passed inout}}
  return y
}


// <rdar://problem/19254812> DI bug when referencing let member of a class
class r19254812Base {}
class r19254812Derived: r19254812Base{
  let pi = 3.14159265359
  
  init(x : ()) {
    markUsed(pi)  // ok, no diagnostic expected.
  }
}


// <rdar://problem/19259730> Using mutating methods in a struct initializer with a let property is rejected
struct StructMutatingMethodTest {
  let a, b: String  // expected-note 2 {{'self.b' not initialized}}
  
  init(x: String, y: String) {
    a = x
    b = y
    mutate()   // ok
  }
  
  init(x: String) {
    a = x
    mutate() // expected-error {{'self' used before all stored properties are initialized}}
    b = x
  }

  init() {
    a = ""
    nonmutate() // expected-error {{'self' used before all stored properties are initialized}}
    b = ""
  }

  mutating func mutate() {}
  func nonmutate() {}
}

 // <rdar://problem/19268443> DI should reject this call to transparent function
 class TransparentFunction {
  let x : Int
  let y : Int
  init() {
    x = 42
    x += 1     // expected-error {{mutating operator '+=' may not be used on immutable value 'self.x'}}

    y = 12
    myTransparentFunction(&y)  // expected-error {{immutable value 'self.y' may not be passed inout}}
  }
}

@_transparent
func myTransparentFunction(_ x : inout Int) {}


// <rdar://problem/19782264> Immutable, optional class members can't have their subproperties read from during init()
class MyClassWithAnInt {
  let channelCount : Int = 42
}
class MyClassTestExample {
  let clientFormat : MyClassWithAnInt!

  init(){
    clientFormat = MyClassWithAnInt()
    _ = clientFormat.channelCount
  }
}


// <rdar://problem/19746552> QoI: variable "used before being initialized" instead of "returned uninitialized" in address-only enum/struct

struct AddressOnlyStructWithInit<T, U> {
  let a : T?
  let b : U?   // expected-note {{'self.b' not initialized}}
  
  init(a : T) {
    self.a = a
  }     // expected-error {{return from initializer without initializing all stored properties}}
}

enum AddressOnlyEnumWithInit<T> {
  case X(T), Y
  
  init() {
  }  // expected-error {{return from enum initializer method without storing to 'self'}}
}


// <rdar://problem/20135113> QoI: enum failable init that doesn't assign to self produces poor error
enum MyAwesomeEnum {
  case One, Two

  init?() {

  }// expected-error {{return from enum initializer method without storing to 'self'}}
}

// <rdar://problem/20679379> DI crashes on initializers on protocol extensions
extension SomeProtocol {
  init?() {
    let a = self  // expected-error {{variable 'self' used before being initialized}}
    self = a
  }

  init(a : Int) {
  } // expected-error {{protocol extension initializer never chained to 'self.init'}}

  init(c : Float) {
    protoMe()   // expected-error {{variable 'self' used before being initialized}}
  } // expected-error {{protocol extension initializer never chained to 'self.init'}}
}



// Lvalue check when the archetypes are not the same.
struct LValueCheck<T> {
  // expected-note @+1 {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
  let x = 0  // expected-note {{initial value already provided in 'let' declaration}}
}

extension LValueCheck {
  init(newY: Int) {
    x = 42  // expected-error {{immutable value 'self.x' may only be initialized once}}
  }
}

// <rdar://problem/20477982> Accessing let-property with default value in init() can throw spurious error
struct DontLoadFullStruct {
  let x: Int = 1
  let y: Int
  init() {
    y = x  // ok!
  }
}


func testReassignment() {
  let c : Int  // expected-note {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
  c = 12
  c = 32  // expected-error {{immutable value 'c' may only be initialized once}}
  _ = c
}


// <rdar://problem/21295093> Swift protocol cannot implement default initializer
protocol ProtocolInitTest {
  init()
  init(a : Int)

  var i: Int { get set }
}

extension ProtocolInitTest {
  init() {
  }  // expected-error {{protocol extension initializer never chained to 'self.init'}}

  init(b : Float) {
    self.init(a: 42)  // ok
  }

  // <rdar://problem/21684596> QoI: Poor DI diagnostic in protocol extension initializer
  init(test1 ii: Int) {
    i = ii         // expected-error {{'self' used before self.init call}}
    self.init()
  }

  init(test2 ii: Int) {
    self = unsafeBitCast(0, to: Self.self)
    i = ii
  }

  init(test3 ii: Int) {
    i = ii                // expected-error {{'self' used before chaining to another self.init requirement}}
    self = unsafeBitCast(0, to: Self.self)
  }

  init(test4 ii: Int) {
    i = ii         // expected-error {{'self' used before chaining to another self.init requirement}}
  }                // expected-error {{protocol extension initializer never chained to 'self.init'}}
}

// <rdar://problem/22436880> Function accepting UnsafeMutablePointer is able to change value of immutable value
func bug22436880(_ x: UnsafeMutablePointer<Int>) {}
func test22436880() {
  let x: Int
  x = 1
  bug22436880(&x) // expected-error {{immutable value 'x' may not be passed inout}}
}

// sr-184
let x: String? // expected-note 2 {{constant defined here}}
print(x?.characters.count as Any) // expected-error {{constant 'x' used before being initialized}}
print(x!) // expected-error {{constant 'x' used before being initialized}}


// <rdar://problem/22723281> QoI: [DI] Misleading error from Swift compiler when using an instance method in init()
protocol PMI {
  func getg()
}

extension PMI {
  func getg() {}
}

class WS: PMI {
  final let x: String  // expected-note {{'self.x' not initialized}}
  
  init() {
    getg()   // expected-error {{use of 'self' in method call 'getg' before all stored properties are initialized}}
    self.x = "foo"
  }
}

// <rdar://problem/23013334> DI QoI: Diagnostic claims that property is being used when it actually isn't
class r23013334 {
  var B: Int   // expected-note {{'self.B' not initialized}}
  var A: String
  
  init(A: String) throws {
    self.A = A
    self.A.withCString { cString -> () in  // expected-error {{'self' captured by a closure before all members were initialized}}

      print(self.A)
      return ()
    }
    
    self.B = 0
  }
  
}

class r23013334Derived : rdar16119509_Base {
  var B: Int   // expected-note {{'self.B' not initialized}}
  var A: String
  
  init(A: String) throws {
    self.A = A
    self.A.withCString { cString -> () in  // expected-error {{'self' captured by a closure before all members were initialized}}
      
      print(self.A)
      return ()
    }
    
    self.B = 0
  }

}

// sr-1469
struct SR1469_Struct1 {
  let a: Int
  let b: Int // expected-note {{'self.b' not initialized}}
  
  init?(x: Int, y: Int) {
    self.a = x
    if y == 42 {
      return // expected-error {{return from initializer without initializing all stored properties}}
    }
    // many lines later
    self.b = y
  }
}

struct SR1469_Struct2 {
  let a: Int
  let b: Int // expected-note {{'self.b' not initialized}}
  
  init?(x: Int, y: Int) {
    self.a = x
    return // expected-error {{return from initializer without initializing all stored properties}}
  }
}

struct SR1469_Struct3 {
  let a: Int
  let b: Int // expected-note {{'self.b' not initialized}}
  
  init?(x: Int, y: Int) {
    self.a = x
    if y == 42 {
      self.b = y
      return
    }
  } // expected-error {{return from initializer without initializing all stored properties}}
}

enum SR1469_Enum1 {
  case A, B
  
  init?(x: Int) {
    if x == 42 {
      return // expected-error {{return from enum initializer method without storing to 'self'}}
    }
    // many lines later
    self = .A
  }
}

enum SR1469_Enum2 {
  case A, B
  
  init?() {
    return // expected-error {{return from enum initializer method without storing to 'self'}}
  }
}
enum SR1469_Enum3 {
  case A, B
  
  init?(x: Int) {
    if x == 42 {
      self = .A
      return
    }
  } // expected-error {{return from enum initializer method without storing to 'self'}}
}

class BadFooSuper {
  init() {}
  init(_ x: BadFooSuper) {}
}

class BadFooSubclass: BadFooSuper {
  override init() {
    super.init(self) // expected-error {{'self' used before super.init call}}
  }
}
