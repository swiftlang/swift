// RUN: %target-parse-verify-swift

// Simple case.
@autoclosure var fn : () -> Int = 4

@autoclosure func func1() {}  // expected-error {{'autoclosure' attribute cannot be applied to this declaration}}
@autoclosure var v1 : Int = 4 // expected-error {{'autoclosure' attribute may only be applied to values of function type}}


func func2(@autoclosure fp : () -> Int) { func2(4)}

func func3(@autoclosure fp fpx : () -> Int) {func3(fp: 0)}
func func4(@autoclosure #fp : () -> Int) {func4(fp: 0)}
func func5(@autoclosure var #fp : () -> Int) {func5(fp: 0)}
func func6(@autoclosure () -> Int) {func6(0)}

// declattr and typeattr on the argument.
func func7(@autoclosure @noreturn () -> Int) {func7(0)}

// autoclosure + inout don't make sense.
func func8(@autoclosure inout x: () -> Bool) -> Bool {  // expected-error {{'autoclosure' attribute may only be applied to values of function type}}
}


// Should have good QoI:
func migrate1(fp fpx : @autoclosure () -> Int) {}   // expected-error {{'autoclosure' attribute is now an attribute of the parameter declaration, not its type}} {{15-15=@autoclosure }} {{24-36=}}
struct MethodHolder {
  func migrate2(a : Int, _ fp : @autoclosure () -> Int) {}    // expected-error {{'autoclosure' attribute is now an attribute of the parameter declaration, not its type}} {{26-26=@autoclosure }} {{33-45=}}
}
func migrate3(#fp : @autoclosure () -> Int) {}    // expected-error {{'autoclosure' attribute is now an attribute of the parameter declaration, not its type}} {{15-15=@autoclosure }} {{21-33=}}
public func || <T: BooleanType>(
  lhs: T, rhs: @autoclosure () -> Bool    // expected-error {{'autoclosure' attribute is now an attribute of the parameter declaration, not its type}} {{11-11=@autoclosure }} {{16-28=}}
  ) -> Bool {
    return lhs.boolValue ? true : rhs().boolValue
}

// <rdar://problem/19707366> QoI: @autoclosure declaration change fixit
let migrate4 : @autoclosure() -> ()   // expected-error {{'autoclosure' attribute is now an attribute of the parameter declaration, not its type}} {{1-1=@autoclosure }} {{16-28=}}


struct SomeStruct {
  @autoclosure let property : () -> Int  // autoclosures work as an property as well.

  init() {
    property = 4
    let a : Int = property()
  }
}

class BaseClass {
  // FIXME: rdar://19311652 - class properties don't work due to synthesized code issues
  @autoclosure var property : () -> Int = 4 // autoclosures work as an property as well.

}

class DerivedClass {
  var property : () -> Int { get {} set {} }
}

protocol P1 {
  typealias Element
}
protocol P2 : P1 {
  typealias Element
}

func overloadedEach<O: P1>(source: O, closure: () -> ()) {
}

func overloadedEach<P: P2>(source: P, closure: () -> ()) {
}

struct S : P2 {
  typealias Element = Int
  func each(@autoclosure closure: () -> ()) {
    overloadedEach(self, closure) // expected-error {{invalid use of non-escaping function in escaping context '() -> ()'}}
      // expected-error@-1 {{cannot find an overload for 'overloadedEach' that accepts an argument list of type '(S, @autoclosure () -> ())'}}
  }
}


// <rdar://problem/19783405> @autoclosure parameters 'escape' from compiler-generated default initilizers
struct AutoclosureEscapeTest {
  // Autoclosure disables synthesis of the default initializer.
  @autoclosure let delayed: () -> Int
}
let _ = AutoclosureEscapeTest(delayed: 4)  // expected-error {{'AutoclosureEscapeTest' cannot be constructed because it has no accessible initializers}}

// @autoclosure(escaping)
func func10(@autoclosure(escaping () -> ()) { } // expected-error{{expected ')' in '@autoclosure' attribute}}
// expected-note@-1{{to match this opening '('}}

func func11(@autoclosure(escaping) @noescape () -> ()) { } // expected-error{{'noescape' attribute conflicts with '@autoclosure(escaping)'}}


class Super {
  func f1(@autoclosure(escaping) x: () -> ()) { }
  func f2(@autoclosure(escaping) x: () -> ()) { }
  func f3(@autoclosure x: () -> ()) { }
}

class Sub : Super {
  override func f1(@autoclosure(escaping) x: () -> ()) { }
  override func f2(@autoclosure x: () -> ()) { } // expected-error{{does not override any method}}
  override func f3(@autoclosure(escaping) x: () -> ()) { }  // expected-error{{does not override any method}}
}

func func12_sink(x: () -> Int) { }

func func12a(@autoclosure x: () -> Int) { 
  func12_sink(x) // expected-error{{invalid use of non-escaping function in escaping context '() -> Int'}}
}
func func12b(@autoclosure(escaping) x: () -> Int) { 
  func12_sink(x)
}

class TestFunc12 {
  var x: Int = 5

  func foo() -> Int { return 0 }

  func test() {
    func12a(x + foo()) // okay
    func12b(x + foo()) 
    // expected-error@-1{{reference to property 'x' in closure requires explicit 'self.' to make capture semantics explicit}}
    // expected-error@-2{{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
  }
}
