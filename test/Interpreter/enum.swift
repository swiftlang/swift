// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test

enum Singleton {
  case x(Int, UnicodeScalar)
}

enum NoPayload {
  case x
  case y
  case z
}

enum SinglePayloadTrivial {
  case x(UnicodeScalar, Int)
  case y
  case z
}

enum MultiPayloadTrivial {
  case x(UnicodeScalar, Int)
  case y(Int, Double)
  case z
}

var s = Singleton.x(1, "a")
switch s {
case .x(var int, var char):
  // CHECK: 1
  print(int)
  // CHECK: a
  print(char)
}

func printNoPayload(_ v: NoPayload) {
  switch v {
  case .x:
    print("NoPayload.x")
  case .y:
    print("NoPayload.y")
  case .z:
    print("NoPayload.z")
  }
}

// CHECK: NoPayload.x
printNoPayload(.x)
// CHECK: NoPayload.y
printNoPayload(.y)
// CHECK: NoPayload.z
printNoPayload(.z)

func printSinglePayloadTrivial(_ v: SinglePayloadTrivial) {
  switch v {
  case .x(let char, let int):
    print("SinglePayloadTrivial.x(\(char), \(int))")
  case .y:
    print("SinglePayloadTrivial.y")
  case .z:
    print("SinglePayloadTrivial.z")
  }
}

// CHECK: SinglePayloadTrivial.x(b, 2)
printSinglePayloadTrivial(.x("b", 2))
// CHECK: SinglePayloadTrivial.y
printSinglePayloadTrivial(.y)
// CHECK: SinglePayloadTrivial.z
printSinglePayloadTrivial(.z)

func printMultiPayloadTrivial(_ v: MultiPayloadTrivial) {
  switch v {
  case .x(let char, let int):
    print("MultiPayloadTrivial.x(\(char), \(int))")
  case .y(let int, let double):
    print("MultiPayloadTrivial.y(\(int), \(double))")
  case .z:
    print("MultiPayloadTrivial.z")
  }
}

// CHECK: MultiPayloadTrivial.x(c, 3)
printMultiPayloadTrivial(.x("c", 3))
// CHECK: MultiPayloadTrivial.y(4, 5.5)
printMultiPayloadTrivial(.y(4, 5.5))
// CHECK: MultiPayloadTrivial.z
printMultiPayloadTrivial(.z)

protocol Runcible {
  func runce()
}

struct Spoon : Runcible {
  var xxx = 0
  func runce() { print("Spoon!") }
}

struct Hat : Runcible {
  var xxx : Float = 0
  func runce() { print("Hat!") }
}

enum SinglePayloadAddressOnly {
  case x(Runcible)
  case y
}

func printSinglePayloadAddressOnly(_ v: SinglePayloadAddressOnly) {
  switch v {
  case .x(let runcible):
    runcible.runce()
  case .y:
    print("Why?")
  }
}

// CHECK: Spoon!
printSinglePayloadAddressOnly(.x(Spoon()))
// CHECK: Hat!
printSinglePayloadAddressOnly(.x(Hat()))
// CHECK: Why?
printSinglePayloadAddressOnly(.y)

enum MultiPayloadAddressOnly {
  case x(Runcible)
  case y(String, Runcible)
  case z
}

func printMultiPayloadAddressOnly(_ v: MultiPayloadAddressOnly) {
  switch v {
  case .x(let runcible):
    runcible.runce()
  case .y(let s, let runcible):
    print("\(s) ", terminator: "")
    runcible.runce()
  case .z:
    print("Zed.")
  }
}

// CHECK: Spoon!
printMultiPayloadAddressOnly(.x(Spoon()))
// CHECK: Porkpie Hat!
printMultiPayloadAddressOnly(.y("Porkpie", Hat()))
// CHECK: Zed.
printMultiPayloadAddressOnly(.z)

enum TrivialGeneric<T, U> {
  case x(T, U)
}

func unwrapTrivialGeneric<T, U>(_ tg: TrivialGeneric<T, U>) -> (T, U) {
  switch tg {
  case .x(let t, let u):
    return (t, u)
  }
}

func wrapTrivialGeneric<T, U>(_ t: T, _ u: U) -> TrivialGeneric<T, U> {
  return .x(t, u)
}

var tg : TrivialGeneric<Int, String> = .x(23, "skidoo")
// CHECK: 23 skidoo
switch tg {
case .x(let t, let u):
  print("\(t) \(u)")
}

// CHECK: 413 dream
switch unwrapTrivialGeneric(.x(413, "dream")) {
case (let t, let u):
  print("\(t) \(u)")
}

// CHECK: 1 is the loneliest number that you'll ever do
switch wrapTrivialGeneric(1, "is the loneliest number that you'll ever do") {
case .x(let t, let u):
  print("\(t) \(u)")
}

enum Ensemble<S : Runcible, H : Runcible> {
  case x(S, H)
}

func concreteEnsemble(_ e: Ensemble<Spoon, Hat>) {
  switch e {
  case .x(let spoon, let hat):
    spoon.runce()
    hat.runce()
  }
}

func genericEnsemble<T : Runcible, U : Runcible>(_ e: Ensemble<T, U>) {
  switch e {
  case .x(let t, let u):
    t.runce()
    u.runce()
  }
}

// CHECK: Spoon!
// CHECK: Hat!
concreteEnsemble(.x(Spoon(), Hat()))
// CHECK: Spoon!
// CHECK: Hat!
genericEnsemble(.x(Spoon(), Hat()))
// CHECK: Spoon!
// CHECK: Spoon!
genericEnsemble(.x(Spoon(), Spoon()))
// CHECK: Hat!
// CHECK: Spoon!
genericEnsemble(.x(Hat(), Spoon()))

enum Optionable<T> {
  case Mere(T)
  case Nought

  init() { self = .Nought }
  init(_ x:T) { self = .Mere(x) }
}

func tryRunce<T : Runcible>(_ x: Optionable<T>) {
  switch x {
  case .Mere(let r):
    r.runce()
  case .Nought:
    print("nought")
  }
}

// CHECK: Spoon!
tryRunce(.Mere(Spoon()))
// CHECK: Hat!
tryRunce(.Mere(Hat()))
// CHECK: nought
tryRunce(Optionable<Spoon>.Nought)
// CHECK: nought
tryRunce(Optionable<Hat>.Nought)
// CHECK: Spoon!
tryRunce(Optionable(Spoon()))
// CHECK: Hat!
tryRunce(Optionable(Hat()))

func optionableInts() {
  let optionables: [Optionable<Int>] = [
    .Mere(219),
    .Nought,
    .Nought,
    .Mere(20721)
  ]

  for o in optionables {
    switch o {
    case .Mere(let x):
      print(x)
    case .Nought:
      print("---")
    }
  }
}
// CHECK-LABEL: Optionable ints:
// CHECK: 219
// CHECK: ---
// CHECK: ---
// CHECK: 20721
print("Optionable ints:")
optionableInts()

enum Suit { case Spades, Hearts, Diamonds, Clubs }

func print(_ suit: Suit) {
  switch suit {
  case .Spades:
    print("♠")
  case .Hearts:
    print("♡")
  case .Diamonds:
    print("♢")
  case .Clubs:
    print("♣")
  }
}

func optionableSuits() {
  let optionables: [Optionable<Suit>] = [
    .Mere(.Spades),
    .Mere(.Diamonds),
    .Nought,
    .Mere(.Hearts)
  ]

  for o in optionables {
    switch o {
    case .Mere(let x):
      print(x)
    case .Nought:
      print("---")
    }
  }
}

// CHECK: ♠
// CHECK: ♢
// CHECK: ---
// CHECK: ♡
optionableSuits()

func optionableRuncibles<T : Runcible>(_ x: T) {
  let optionables: [Optionable<T>] = [
    .Mere(x),
    .Nought,
    .Mere(x),
    .Nought
  ]

  for o in optionables {
    switch o {
    case .Mere(let x):
      x.runce()
    case .Nought:
      print("---")
    }
  }
}
// CHECK: Spoon!
// CHECK: ---
// CHECK: Spoon!
// CHECK: ---
optionableRuncibles(Spoon())
// CHECK: Hat!
// CHECK: ---
// CHECK: Hat!
// CHECK: ---
optionableRuncibles(Hat())

// <rdar://problem/15383966>

protocol ClassProtocol : class {}

class Rdar15383966 : ClassProtocol
{
    var id : Int

    init(_ anID : Int) {
      print("X(\(anID))")
      id = anID
    }

    deinit {
       print("~X(\(id))")
    }
}


func generic<T>(_ x: T?) { }
func generic_over_classes<T : ClassProtocol>(_ x: T?) { }
func nongeneric(_ x: Rdar15383966?) { }

func test_generic()
{
    var x: Rdar15383966? = Rdar15383966(1)
    generic(x)
    x = .none
    generic(x)
}
// CHECK: X(1)
// CHECK: ~X(1)
test_generic()

func test_nongeneric()
{
    var x: Rdar15383966? = Rdar15383966(2)
    nongeneric(x)
    x = .none
    nongeneric(x)
}
// CHECK: X(2)
// CHECK: ~X(2)
test_nongeneric()

func test_generic_over_classes()
{
    var x: Rdar15383966? = Rdar15383966(3)
    generic_over_classes(x)
    x = .none
    generic_over_classes(x)
}
// CHECK: X(3)
// CHECK: ~X(3)
test_generic_over_classes()

struct S { 
  var a: Int32; var b: Int64 

  init(_ a: Int32, _ b: Int64) {
    self.a = a
    self.b = b
  }
}

enum MultiPayloadSpareBitAggregates {
  case x(Int32, Int64)
  case y(Rdar15383966, Rdar15383966)
  case z(S)
}

func test_spare_bit_aggregate(_ x: MultiPayloadSpareBitAggregates) {
  switch x {
  case .x(let i32, let i64):
    print(".x(\(i32), \(i64))")
  case .y(let a, let b):
    print(".y(\(a.id), \(b.id))")
  case .z(let s):
    print(".z(\(s))")
  }
}

print("---")
// CHECK: .x(22, 44)
test_spare_bit_aggregate(.x(22, 44))
// CHECK: X(222)
// CHECK: X(444)
// CHECK: .y(222, 444)
// CHECK-DAG: ~X(222)
// CHECK-DAG: ~X(444)
test_spare_bit_aggregate(.y(Rdar15383966(222), Rdar15383966(444)))
// CHECK: .z(S(a: 333, b: 666))
test_spare_bit_aggregate(.z(S(333, 666)))

print("---")
struct OptionalTuple<T> {
  var value : (T, T)?

  init(_ value: (T, T)?) {
    self.value = value
  }
}
func test_optional_generic_tuple<T>(_ a: OptionalTuple<T>) -> T {
  print("optional pair is same size as pair: \(MemoryLayout.size(ofValue: a) == MemoryLayout<T>.size*2)")
  return a.value!.0
}
print("Int result: \(test_optional_generic_tuple(OptionalTuple<Int>((5, 6))))")
// CHECK: optional pair is same size as pair: false
// CHECK: Int result: 5

class AnyOldClass {
  var x: Int
  init(_ value: Int) { x = value }
}
print("class result: \(test_optional_generic_tuple(OptionalTuple((AnyOldClass(10), AnyOldClass(11)))).x)")
// CHECK: optional pair is same size as pair: true
// CHECK: class result: 10

// <rdar://problem/16887421>
// CHECK-LABEL: Optional equality:
print("Optional equality:")
// CHECK: true
print((NoPayload.x as NoPayload?) == NoPayload.x)
// CHECK: false
print((NoPayload.x as NoPayload?) == NoPayload.y)

// rdar://problem/17814752
class Foo {}

struct Oof {
  weak var foo: Foo?
}

protocol Boo {}

struct Goof {
  var boo: Boo?
}

let oofs = [Oof()]
let goofs = [Goof()]

enum Planet: String {
    case Mercury = "Mercury", Venus = "Venus", Mars
    init() {
        self = Planet.Venus
    }
}
var pl1 = Planet()
print(pl1.rawValue)
// CHECK: {{^Venus$}}
print(Planet.Mars.rawValue)
// CHECK: {{^Mars$}}

enum EitherOr<T, U> {
  case Left(T)
  case Middle
  case Center
  case Right(U)
}

@inline(never)
func presentEitherOr<T, U>(_ e: EitherOr<T, U>) {
  switch e {
  case .Left(let l):
    print("Left(\(l))")
  case .Right(let r):
    print("Right(\(r))")
  case .Middle:
    print("Middle")
  case .Center:
    print("Center")
  }
}

@inline(never)
func presentEitherOrsOf<T, U>(t: T, u: U) {
  presentEitherOr(EitherOr<T, U>.Left(t))
  presentEitherOr(EitherOr<T, U>.Middle)
  presentEitherOr(EitherOr<T, U>.Center)
  presentEitherOr(EitherOr<T, U>.Right(u))
}

presentEitherOr(EitherOr<(), ()>.Left(()))  // CHECK-NEXT: Left(())
presentEitherOr(EitherOr<(), ()>.Middle)  // CHECK-NEXT: Middle
presentEitherOr(EitherOr<(), ()>.Center)  // CHECK-NEXT: Center
presentEitherOr(EitherOr<(), ()>.Right(())) // CHECK-NEXT: Right(())

// CHECK-NEXT: Left(())
// CHECK-NEXT: Middle
// CHECK-NEXT: Center
// CHECK-NEXT: Right(())
presentEitherOrsOf(t: (), u: ())

presentEitherOr(EitherOr<Int, String>.Left(1))      // CHECK-NEXT: Left(1)
presentEitherOr(EitherOr<Int, String>.Middle)       // CHECK-NEXT: Middle
presentEitherOr(EitherOr<Int, String>.Center)       // CHECK-NEXT: Center
presentEitherOr(EitherOr<Int, String>.Right("foo")) // CHECK-NEXT: Right(foo)

// CHECK-NEXT: Left(1)
// CHECK-NEXT: Middle
// CHECK-NEXT: Center
// CHECK-NEXT: Right(foo)
presentEitherOrsOf(t: 1, u: "foo")

presentEitherOr(EitherOr<(), String>.Left(()))       // CHECK-NEXT: Left(())
presentEitherOr(EitherOr<(), String>.Middle)       // CHECK-NEXT: Middle
presentEitherOr(EitherOr<(), String>.Center)       // CHECK-NEXT: Center
presentEitherOr(EitherOr<(), String>.Right("foo")) // CHECK-NEXT: Right(foo)

// CHECK-NEXT: Left(())
// CHECK-NEXT: Middle
// CHECK-NEXT: Center
// CHECK-NEXT: Right(foo)
presentEitherOrsOf(t: (), u: "foo")

// SR-5148
enum Payload {
    case email
}
enum Test {
    case a
    indirect case b(Payload)
}

@inline(never)
func printA() {
    print("an a")
}

@inline(never)
func printB() {
    print("an b")
}

@inline(never)
func testCase(_ testEmail: Test) {
  switch testEmail {
    case .a:
      printA()
    case .b:
      printB()
  }
}

@inline(never)
func createTestB() -> Test  {
  return Test.b(.email)
}

@inline(never)
func createTestA() -> Test  {
  return Test.a
}

// CHECK-NEXT: an b
testCase(createTestB())
// CHECK-NEXT: b(a.Payload.email)
print(createTestB())
// CHECK-NEXT: a
print(createTestA())
// CHECK-NEXT: done
print("done")

public enum MyOptional<T> {
  case Empty
  case SecondEmpty
  case Some(T)
}

public class StopSpecialization {
  public func generate<T>(_ e: T) -> MyOptional<T> {
    return MyOptional.Some(e)
  }
  public func generate2<T>(_ e: T) -> MyOptional<T> {
    return MyOptional.Empty
  }
}

@inline(never)
func test(_ s : StopSpecialization, _ N: Int) -> Bool {
  let x = s.generate(N)
  switch x {
    case .SecondEmpty:
      return false
    case .Empty:
      return false
    case .Some(_):
      return true
  }
}

@inline(never)
func test2(_ s : StopSpecialization, _ N: Int) -> Bool {
  let x = s.generate2(N)
  switch x {
    case .SecondEmpty:
      return false
    case .Empty:
      return true
    case .Some(_):
      return false
  }
}

@inline(never)
func run() {
// CHECK: true
  print(test(StopSpecialization(), 12))
// CHECK: true
  print(test2(StopSpecialization(), 12))
}

run()

public enum Indirect<T> {
  indirect case payload((T, other: T))
  case none
}

public func testIndirectEnum<T>(_ payload: T) -> Indirect<T> {
  return Indirect.payload((payload, other: payload))
}

public func testCase(_ closure: @escaping (Int) -> ()) -> Indirect<(Int) -> ()> {
  return testIndirectEnum(closure)
}

// CHECK: payload((Function), other: (Function))
print(testCase({ _ in }))


enum MultiIndirectRef {
  case empty
  indirect case ind(Int)
  case collection([Int])
}

struct Container {
  var storage : MultiIndirectRef = .empty

  mutating func adoptStyle(_ s: Int) {
    storage = .ind(s)
  }
}

func copyStorage(_ s: Int, _ x : Container) -> Container {
  var c = x
  c.adoptStyle(s)
  return c
}

func testCase() {
  let l = Container()
  let c = copyStorage(5, l)
  print(c)
}

// CHECK: Container(storage: a.MultiIndirectRef.ind(5))
testCase()
