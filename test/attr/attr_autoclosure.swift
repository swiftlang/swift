// RUN: %target-parse-verify-swift

// Simple case.
@autoclosure var fn : () -> Int = 4  // expected-error {{@autoclosure may only be used on 'parameter' declarations}} expected-error {{'Int' is not convertible to '() -> Int'}}

@autoclosure func func1() {}  // expected-error {{@autoclosure may only be used on 'parameter' declarations}}

func func1a(@autoclosure v1 : Int) {} // expected-error {{@autoclosure may only be applied to values of function type}}


func func2(@autoclosure fp : () -> Int) { func2(4)}

func func3(@autoclosure fp fpx : () -> Int) {func3(fp: 0)}
func func4(@autoclosure fp fp : () -> Int) {func4(fp: 0)}
func func5(@autoclosure var fp fp : () -> Int) {func5(fp: 0)} // expected-warning {{parameter 'fp' was never mutated}}
func func6(@autoclosure () -> Int) {func6(0)}

// declattr and typeattr on the argument.
func func7(@autoclosure @noreturn () -> Int) {func7(0)}

// autoclosure + inout don't make sense.
func func8(@autoclosure inout x: () -> Bool) -> Bool {  // expected-error {{@autoclosure may only be applied to values of function type}}
}


// Should have good QoI:
func migrate1(fp fpx : @autoclosure () -> Int) {}   // expected-error {{@autoclosure is now an attribute of the parameter declaration, not its type}} {{15-15=@autoclosure }} {{24-37=}}
struct MethodHolder {
  func migrate2(a : Int, _ fp : @autoclosure () -> Int) {}    // expected-error {{@autoclosure is now an attribute of the parameter declaration, not its type}} {{26-26=@autoclosure }} {{33-46=}}
}
func migrate3(fp fp : @autoclosure () -> Int) {}    // expected-error {{@autoclosure is now an attribute of the parameter declaration, not its type}} {{15-15=@autoclosure }} {{23-36=}}
public func || <T: BooleanType>(
  lhs: T, rhs: @autoclosure () -> Bool    // expected-error {{@autoclosure is now an attribute of the parameter declaration, not its type}} {{11-11=@autoclosure }} {{16-29=}}
  ) -> Bool {
    return lhs.boolValue ? true : rhs().boolValue
}

// <rdar://problem/19707366> QoI: @autoclosure declaration change fixit
let migrate4 : @autoclosure() -> ()   // expected-error {{@autoclosure is now an attribute of the parameter declaration, not its type}} {{1-1=@autoclosure }} {{16-28=}}


struct SomeStruct {
  @autoclosure let property : () -> Int  // expected-error {{@autoclosure may only be used on 'parameter' declarations}}

  init() {
  }
}

class BaseClass {
  @autoclosure var property : () -> Int // expected-error {{@autoclosure may only be used on 'parameter' declarations}}
  init() {}
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

func overloadedEach<O: P1>(source: O, _ closure: () -> ()) {
}

func overloadedEach<P: P2>(source: P, _ closure: () -> ()) {
}

struct S : P2 {
  typealias Element = Int
  func each(@autoclosure closure: () -> ()) {
    overloadedEach(self, closure) // expected-error {{invalid use of non-escaping function in escaping context '() -> ()'}}
      // expected-error@-1 {{cannot find an overload for 'overloadedEach' that accepts an argument list of type '(S, @autoclosure () -> ())'}}
  }
}


struct AutoclosureEscapeTest {
  @autoclosure let delayed: () -> Int  // expected-error {{@autoclosure may only be used on 'parameter' declarations}}
}

// @autoclosure(escaping)
func func10(@autoclosure(escaping () -> ()) { } // expected-error{{expected ')' in @autoclosure}}
// expected-note@-1{{to match this opening '('}}

func func11(@autoclosure(escaping) @noescape () -> ()) { } // expected-error{{@noescape conflicts with @autoclosure(escaping)}}


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


enum AutoclosureFailableOf<T> {
  case Success(@autoclosure () -> T)  // expected-error {{@autoclosure is only allowed on parameters, not on enum cases}}
  case Failure()
}


