// RUN: %target-typecheck-verify-swift

//===----------------------------------------------------------------------===//
// Tests for various simple enum constructs
//===----------------------------------------------------------------------===//


public enum unionSearchFlags {
  case none
  case backwards
  case anchored

  init() { self = .none }
}

func test1() -> unionSearchFlags {
  let _ : unionSearchFlags
  var b = unionSearchFlags.none
  b = unionSearchFlags.anchored
  _ = b

  return unionSearchFlags.backwards
}

func test1a() -> unionSearchFlags {
  var _ : unionSearchFlags
  var b : unionSearchFlags = .none
  b = .anchored
  _ = b

  // ForwardIndex use of MaybeInt.
  _ = MaybeInt.none

  return .backwards
}

func test1b(_ b : Bool) {
  _ = 123
  _ = .description == 1 // expected-error {{ambiguous reference to member '=='}} 
}

enum MaybeInt {
  case none
  case some(Int) // expected-note {{'some' declared here}}

  init(_ i: Int) { self = MaybeInt.some(i) }
}

func test2(_ a: Int, _ b: Int, _ c: MaybeInt) {
  _ = MaybeInt.some(4)
  _ = MaybeInt.some
  _ = MaybeInt.some(b)

  test2(1, 2, .none)
}

enum ZeroOneTwoThree {
  case Zero
  case One(Int)
  case Two(Int, Int)
  case Three(Int,Int,Int)
  case Unknown(MaybeInt, MaybeInt, MaybeInt)

  init (_ i: Int) { self = .One(i) }
  init (_ i: Int, _ j: Int, _ k: Int) { self = .Three(i, j, k) }
  init (_ i: MaybeInt, _ j: MaybeInt, _ k: MaybeInt) { self = .Unknown(i, j, k) }
}

func test3(_ a: ZeroOneTwoThree) {
  _ = ZeroOneTwoThree.Three(1,2,3)
  _ = ZeroOneTwoThree.Unknown(
    MaybeInt.none, MaybeInt.some(4), MaybeInt.some(32))
  _ = ZeroOneTwoThree(MaybeInt.none, MaybeInt(4), MaybeInt(32))

  var _ : Int =
     ZeroOneTwoThree.Zero // expected-error {{cannot convert value of type 'ZeroOneTwoThree' to specified type 'Int'}}

  // expected-warning @+1 {{unused}}
  test3 ZeroOneTwoThree.Zero // expected-error {{expression resolves to an unused function}} expected-error{{consecutive statements}} {{8-8=;}}
  test3 (ZeroOneTwoThree.Zero)
  test3(ZeroOneTwoThree.Zero)
  test3 // expected-error {{expression resolves to an unused function}}
  // expected-warning @+1 {{unused}}
  (ZeroOneTwoThree.Zero)
  
  var _ : ZeroOneTwoThree = .One(4)
  
  var _ : (Int,Int) -> ZeroOneTwoThree = .Two // expected-error{{type '(Int, Int) -> ZeroOneTwoThree' has no member 'Two'}}
  var _ : Int = .Two // expected-error{{type 'Int' has no member 'Two'}}
  var _ : MaybeInt = 0 > 3 ? .none : .soma(3) // expected-error {{type 'MaybeInt' has no member 'soma'; did you mean 'some'?}}
}

func test3a(_ a: ZeroOneTwoThree) {
  var e : ZeroOneTwoThree = (.Three(1, 2, 3))
  var f = ZeroOneTwoThree.Unknown(.none, .some(4), .some(32))

  var g = .none  // expected-error {{reference to member 'none' cannot be resolved without a contextual type}}

  // Overload resolution can resolve this to the right constructor.
  var h = ZeroOneTwoThree(1)
  
  var i = 0 > 3 ? .none : .some(3) // expected-error {{reference to member 'none' cannot be resolved without a contextual type}}
  
  test3a;  // expected-error {{unused function}}
  .Zero   // expected-error {{reference to member 'Zero' cannot be resolved without a contextual type}}
  test3a   // expected-error {{unused function}}
  (.Zero) // expected-error {{reference to member 'Zero' cannot be resolved without a contextual type}}
  test3a(.Zero)
}


struct CGPoint { var x : Int, y : Int }
typealias OtherPoint = (x : Int, y : Int)

func test4() {
  var a : CGPoint
  // Note: we reject the following because it conflicts with the current
  // "init" hack.
  var b = CGPoint.CGPoint(1, 2) // expected-error {{type 'CGPoint' has no member 'CGPoint'}}
  var c = CGPoint(x: 2, y : 1)   // Using injected name.

  var e = CGPoint.x // expected-error {{member 'x' cannot be used on type 'CGPoint'}}
  var f = OtherPoint.x  // expected-error {{type 'OtherPoint' (aka '(x: Int, y: Int)') has no member 'x'}}
}



struct CGSize { var width : Int, height : Int }

extension CGSize {
  func area() -> Int {
    return width*self.height
  }
  
  func area_wrapper() -> Int {
    return area()
  }
}

struct CGRect { 
  var origin : CGPoint,
  size : CGSize
  
  func area() -> Int {
    return self.size.area()
  }
}

func area(_ r: CGRect) -> Int {
  return r.size.area()
}

extension CGRect {
  func search(_ x: Int) -> CGSize {}
  func bad_search(_: Int) -> CGSize {}
}

func test5(_ myorigin: CGPoint) {
  let x1 = CGRect(origin: myorigin, size: CGSize(width: 42, height: 123))
  let x2 = x1

  _ = 4+5

  // Dot syntax.
  _ = x2.origin.x
  _ = x1.size.area()
  _ = (r : x1.size).r.area() // expected-error {{cannot create a single-element tuple with an element label}}
  _ = x1.size.area()
  _ = (r : x1.size).r.area() // expected-error {{cannot create a single-element tuple with an element label}}
  
  _ = x1.area

  _ = x1.search(42)
  _ = x1.search(42).width

  // TODO: something like this (name binding on the LHS):
  // var (CGSize(width, height)) = CGSize(1,2)

  // TODO: something like this, how do we get it in scope in the {} block?
  //if (var some(x) = somemaybeint) { ... }

  
}

struct StructTest1 {
  var a : Int, c, b : Int


  typealias ElementType = Int
}

enum UnionTest1 {
  case x
  case y(Int)

  func foo() {}

  init() { self = .x }
}


extension UnionTest1 {
  func food() {}
  func bar() {}

  // Type method.
  static func baz() {}
}

struct EmptyStruct {
  func foo() {}
}

func f() { 
  let a : UnionTest1
  a.bar()
  UnionTest1.baz()  // dot syntax access to a static method.
  
  // Test that we can get the "address of a member".
  var _ : () -> () = UnionTest1.baz
  var _ : (UnionTest1) -> () -> () = UnionTest1.bar
}

func union_error(_ a: ZeroOneTwoThree) {
  var _ : ZeroOneTwoThree = .Zero(1) // expected-error {{enum case 'Zero' has no associated values}}
  var _ : ZeroOneTwoThree = .Zero() // expected-error {{enum case 'Zero' has no associated values}} {{34-36=}}
  var _ : ZeroOneTwoThree = .One // expected-error {{member 'One' expects argument of type 'Int'}}
  var _ : ZeroOneTwoThree = .foo // expected-error {{type 'ZeroOneTwoThree' has no member 'foo'}}
  var _ : ZeroOneTwoThree = .foo() // expected-error {{type 'ZeroOneTwoThree' has no member 'foo'}}
}

func local_struct() {
  struct s { func y() {} }
}

//===----------------------------------------------------------------------===//
// A silly units example showing "user defined literals".
//===----------------------------------------------------------------------===//

struct distance { var v : Int }

func - (lhs: distance, rhs: distance) -> distance {}

extension Int {
  func km() -> enumtest.distance {}
  func cm() -> enumtest.distance {}
}

func units(_ x: Int) -> distance {
  return x.km() - 4.cm() - 42.km()
}



var %% : distance -> distance // expected-error {{expected pattern}} 

func badTupleElement() {
  typealias X = (x : Int, y : Int)
  var y = X.y // expected-error{{type 'X' (aka '(x: Int, y: Int)') has no member 'y'}}
  var z = X.z // expected-error{{type 'X' (aka '(x: Int, y: Int)') has no member 'z'}}
}

enum Direction {
  case North(distance: Int)
  case NorthEast(distanceNorth: Int, distanceEast: Int)
}

func testDirection() {
  var dir: Direction = .North(distance: 5)
  dir = .NorthEast(distanceNorth: 5, distanceEast: 7)

  var i: Int
  switch dir {
  case .North(let x):
    i = x
    break

  case .NorthEast(let x): // expected-warning {{cannot match several associated values at once, implicitly tupling the associated values and trying to match that instead}}
    i = x.distanceEast
    break
  }
  _ = i
}

enum NestedSingleElementTuple {
  case Case(x: (y: Int)) // expected-error{{cannot create a single-element tuple with an element label}} {{17-20=}}
}

enum SimpleEnum {
  case X, Y
}

func testSimpleEnum() {
  let _ : SimpleEnum = .X
  let _ : SimpleEnum = (.X)
  let _ : SimpleEnum=.X    // expected-error {{'=' must have consistent whitespace on both sides}}
}

enum SR510: String {
    case Thing = "thing"
    case Bob = {"test"} // expected-error {{raw value for enum case must be a literal}}
}


// <rdar://problem/21269142> Diagnostic should say why enum has no .rawValue member
enum E21269142 {  // expected-note {{did you mean to specify a raw type on the enum declaration?}}
  case Foo
}

print(E21269142.Foo.rawValue)  // expected-error {{value of type 'E21269142' has no member 'rawValue'}}

// Check that typo correction does something sensible with synthesized members.
enum SyntheticMember { // expected-note {{property 'hashValue' is implicitly declared}}
  case Foo
}

func useSynthesizedMember() {
  print(SyntheticMember.Foo.hasValue) // expected-error {{value of type 'SyntheticMember' has no member 'hasValue'; did you mean 'hashValue'?}}
}

// Non-materializable argument type
enum Lens<T> {
  case foo(inout T) // expected-error {{'inout' may only be used on parameters}}
  case bar(inout T, Int) // expected-error {{'inout' may only be used on parameters}}

  case baz((inout T) -> ()) // ok
  case quux((inout T, inout T) -> ()) // ok
}

// In the long term, these should be legal, but we don't support them right
// now and we shouldn't pretend to.
// rdar://46684504
enum HasVariadic {
  case variadic(x: Int...) // expected-error {{variadic enum cases are not supported}}
}

// SR-2176
enum Foo {
  case bar
  case none
}

let _: Foo? = .none // expected-warning {{assuming you mean 'Optional<Foo>.none'; did you mean 'Foo.none' instead?}}
// expected-note@-1 {{explicitly specify 'Optional' to silence this warning}} {{15-15=Optional}}
// expected-note@-2 {{use 'Foo.none' instead}} {{15-15=Foo}}
let _: Foo?? = .none // expected-warning {{assuming you mean 'Optional<Optional<Foo>>.none'; did you mean 'Foo.none' instead?}}
// expected-note@-1 {{explicitly specify 'Optional' to silence this warning}} {{16-16=Optional}}
// expected-note@-2 {{use 'Foo.none' instead}} {{16-16=Foo}}

let _: Foo = .none // ok
let _: Foo = .bar // ok
let _: Foo? = .bar // ok
let _: Foo?? = .bar // ok
let _: Foo = Foo.bar // ok
let _: Foo = Foo.none // ok
let _: Foo? = Foo.none // ok
let _: Foo?? = Foo.none // ok

func baz(_: Foo?) {}
baz(.none) // expected-warning {{assuming you mean 'Optional<Foo>.none'; did you mean 'Foo.none' instead?}}
// expected-note@-1 {{explicitly specify 'Optional' to silence this warning}} {{5-5=Optional}}
// expected-note@-2 {{use 'Foo.none' instead}} {{5-5=Foo}}

let test: Foo? = .none // expected-warning {{assuming you mean 'Optional<Foo>.none'; did you mean 'Foo.none' instead?}}
// expected-note@-1 {{explicitly specify 'Optional' to silence this warning}} {{18-18=Optional}}
// expected-note@-2 {{use 'Foo.none' instead}} {{18-18=Foo}}
let answer = test == .none // expected-warning {{assuming you mean 'Optional<Foo>.none'; did you mean 'Foo.none' instead?}}
// expected-note@-1 {{explicitly specify 'Optional' to silence this warning}} {{22-22=Optional}}
// expected-note@-2 {{use 'Foo.none' instead}} {{22-22=Foo}}

enum Bar {
  case baz
}

let _: Bar? = .none // ok
let _: Bar?? = .none // ok
let _: Bar? = .baz // ok
let _: Bar?? = .baz // ok
let _: Bar = .baz // ok

enum AnotherFoo {
  case none(Any)
}

let _: AnotherFoo? = .none // ok
let _: AnotherFoo? = .none(0) // ok

struct FooStruct {
  static let none = FooStruct()
  static let one = FooStruct()
}

let _: FooStruct? = .none // expected-warning {{assuming you mean 'Optional<FooStruct>.none'; did you mean 'FooStruct.none' instead?}}
// expected-note@-1 {{explicitly specify 'Optional' to silence this warning}} {{21-21=Optional}}
// expected-note@-2 {{use 'FooStruct.none' instead}} {{21-21=FooStruct}}
let _: FooStruct?? = .none // expected-warning {{assuming you mean 'Optional<Optional<FooStruct>>.none'; did you mean 'FooStruct.none' instead?}}
// expected-note@-1 {{explicitly specify 'Optional' to silence this warning}} {{22-22=Optional}}
// expected-note@-2 {{use 'FooStruct.none' instead}} {{22-22=FooStruct}}
let _: FooStruct = .none // ok
let _: FooStruct = .one // ok
let _: FooStruct? = .one // ok
let _: FooStruct?? = .one // ok

struct NestedBazEnum {
  enum Baz {
    case one
    case none
  }
}

let _: NestedBazEnum.Baz? = .none // expected-warning {{assuming you mean 'Optional<NestedBazEnum.Baz>.none'; did you mean 'NestedBazEnum.Baz.none' instead?}}
// expected-note@-1 {{explicitly specify 'Optional' to silence this warning}} {{29-29=Optional}}
// expected-note@-2 {{use 'NestedBazEnum.Baz.none' instead}} {{29-29=NestedBazEnum.Baz}}
let _: NestedBazEnum.Baz?? = .none // expected-warning {{assuming you mean 'Optional<Optional<NestedBazEnum.Baz>>.none'; did you mean 'NestedBazEnum.Baz.none' instead?}}
// expected-note@-1 {{explicitly specify 'Optional' to silence this warning}} {{30-30=Optional}}
// expected-note@-2 {{use 'NestedBazEnum.Baz.none' instead}} {{30-30=NestedBazEnum.Baz}}
let _: NestedBazEnum.Baz = .none // ok
let _: NestedBazEnum.Baz = .one // ok
let _: NestedBazEnum.Baz? = .one // ok
let _: NestedBazEnum.Baz?? = .one // ok

struct NestedBazEnumGeneric {
  enum Baz<T> {
    case one
    case none
  }
}

let _: NestedBazEnumGeneric.Baz<Int>? = .none // expected-warning {{assuming you mean 'Optional<NestedBazEnumGeneric.Baz<Int>>.none'; did you mean 'NestedBazEnumGeneric.Baz<Int>.none' instead?}}
// expected-note@-1 {{explicitly specify 'Optional' to silence this warning}} {{41-41=Optional}}
// expected-note@-2 {{use 'NestedBazEnumGeneric.Baz<Int>.none' instead}} {{41-41=NestedBazEnumGeneric.Baz<Int>}}
let _: NestedBazEnumGeneric.Baz<Int>?? = .none // expected-warning {{assuming you mean 'Optional<Optional<NestedBazEnumGeneric.Baz<Int>>>.none'; did you mean 'NestedBazEnumGeneric.Baz<Int>.none' instead?}}
// expected-note@-1 {{explicitly specify 'Optional' to silence this warning}} {{42-42=Optional}}
// expected-note@-2 {{use 'NestedBazEnumGeneric.Baz<Int>.none' instead}} {{42-42=NestedBazEnumGeneric.Baz<Int>}}
let _: NestedBazEnumGeneric.Baz<Int> = .none // ok
let _: NestedBazEnumGeneric.Baz<Int> = .one // ok
let _: NestedBazEnumGeneric.Baz<Int>? = .one // ok
let _: NestedBazEnumGeneric.Baz<Int>?? = .one // ok

class C {}
protocol P {}

enum E : C & P {}
// expected-error@-1 {{inheritance from class-constrained protocol composition type 'C & P'}}