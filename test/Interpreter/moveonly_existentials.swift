// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g %s -o %t/bin
// RUN: %target-codesign %t/bin
// RUN: %target-run %t/bin | %FileCheck %s

// REQUIRES: executable_test

protocol Boopable: ~Copyable {
  func boop()
  mutating func bonk()
}

struct S: ~Copyable, Boopable {
  func boop() { print("boop") }
  mutating func bonk() { print("hmm") }
}

func borrow(_ b: borrowing any Boopable & ~Copyable) {
  b.boop()
}

func mutate(_ b: inout any Boopable & ~Copyable) {
  b.bonk()
}

// CHECK: boop
// CHECK: hmm
borrow(S())
var s = S() as any Boopable & ~Copyable
mutate(&s)

// A `consuming` requirement called on an existential held in storage -- a
// parameter, a `var`, or a stored property -- must consume the payload in
// place. The deinit prints let us see that each payload is destroyed exactly
// once, so a stray copy or a missed destroy would show up here.

protocol Consumable: ~Copyable {
  consuming func eat()
  borrowing func peek()
}

struct Meal: ~Copyable, Consumable {
  let name: String
  init(_ name: String) { self.name = name }
  consuming func eat() { print("eat \(name)") }
  borrowing func peek() { print("peek \(name)") }
  deinit { print("deinit \(name)") }
}

func make(_ name: String) -> any Consumable & ~Copyable { Meal(name) }

func consumeParam(_ x: consuming any Consumable & ~Copyable) {
  x.eat()
}

// A borrowing call must not consume the existential.
func peekThenEat(_ x: consuming any Consumable & ~Copyable) {
  x.peek()
  x.peek()
  x.eat()
}

struct Box: ~Copyable {
  var item: any Consumable & ~Copyable
}

func consumeStoredProperty(_ b: consuming Box) {
  b.item.eat()
}

func consumeLocalVar() {
  var x: any Consumable & ~Copyable = make("d")
  x = make("e")
  x.eat()
}

// CHECK-NEXT: eat a
// CHECK-NEXT: deinit a
consumeParam(make("a"))

// CHECK-NEXT: peek b
// CHECK-NEXT: peek b
// CHECK-NEXT: eat b
// CHECK-NEXT: deinit b
peekThenEat(make("b"))

// CHECK-NEXT: eat c
// CHECK-NEXT: deinit c
consumeStoredProperty(Box(item: make("c")))

// CHECK-NEXT: deinit d
// CHECK-NEXT: eat e
// CHECK-NEXT: deinit e
consumeLocalVar()
