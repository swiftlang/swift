// RUN: %target-typecheck-verify-swift

func foo(_ a: Int) {
  // expected-error @+1 {{invalid character in source file}} {{8-9= }}
  foo(<\a\>) // expected-error {{invalid character in source file}} {{10-11= }}
  // expected-error @-1 {{'<' is not a prefix unary operator}}
  // expected-error @-2 {{'>' is not a postfix unary operator}}
}

// rdar://15946844
func test1(inout var x : Int) {}  // expected-error {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}} {{18-22=}}
// expected-error @-1 {{'inout' before a parameter name is not allowed, place it before the parameter type instead}} {{12-17=}} {{26-26=inout }}
func test2(inout let x : Int) {}  // expected-error {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}} {{18-22=}}
// expected-error @-1 {{'inout' before a parameter name is not allowed, place it before the parameter type instead}} {{12-17=}} {{26-26=inout }}
func test3(f : (inout _ x : Int) -> Void) {} // expected-error {{'inout' before a parameter name is not allowed, place it before the parameter type instead}}

func test3() {
  undeclared_func( // expected-error {{use of unresolved identifier 'undeclared_func'}}
} // expected-error {{expected expression in list of expressions}}

func runAction() {} // expected-note {{did you mean 'runAction'?}}

// rdar://16601779
func foo() {
  runAction(SKAction.sequence() // expected-error {{use of unresolved identifier 'SKAction'}} expected-error {{expected ',' separator}} {{32-32=,}}
    
    skview!
    // expected-error @-1 {{use of unresolved identifier 'skview'}}
}

super.init() // expected-error {{'super' cannot be used outside of class members}}

switch state { // expected-error {{use of unresolved identifier 'state'}}
  let duration : Int = 0 // expected-error {{all statements inside a switch must be covered by a 'case' or 'default'}}
  case 1:
    break
}

func testNotCoveredCase(x: Int) {
  switch x {
    let y = "foo" // expected-error {{all statements inside a switch must be covered by a 'case' or 'default'}}
    switch y {
      case "bar":
        blah blah // ignored
    }
  case "baz": // expected-error {{expression pattern of type 'String' cannot match values of type 'Int'}}
    break
  case 1:
    break
  default:
    break
  }

  switch x {
#if true // expected-error {{all statements inside a switch must be covered by a 'case' or 'default'}}
  case 1:
    break
  case "foobar": // ignored
    break
  default:
    break
#endif
  } // expected-error{{'switch' statement body must have at least one 'case' or 'default' block}}
}

// rdar://18926814
func test4() {
  let abc = 123
  _ = " >> \( abc } ) << " // expected-error {{expected ',' separator}} {{18-18=,}}  expected-error {{expected expression in list of expressions}}  expected-error {{extra tokens after interpolated string expression}}

}

// rdar://problem/18507467
func d(_ b: String -> <T>() -> T) {} // expected-error {{expected type for function result}}


// <rdar://problem/22143680> QoI: terrible diagnostic when trying to form a generic protocol
protocol Animal<Food> {  // expected-error {{protocols do not allow generic parameters; use associated types instead}}
  func feed(_ food: Food) // expected-error {{use of undeclared type 'Food'}}
}



// SR-573 - Crash with invalid parameter declaration
class Starfish {}
struct Salmon {}
func f573(s Starfish,  // expected-error {{parameter requires an explicit type}}
          _ ss: Salmon) -> [Int] {}
func g573() { f573(Starfish(), Salmon()) }

func SR698(_ a: Int, b: Int) {}
SR698(1, b: 2,) // expected-error {{unexpected ',' separator}}

// SR-979 - Two inout crash compiler
func SR979a(a : inout inout Int) {}  // expected-error {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}} {{17-23=}}
func SR979b(inout inout b: Int) {} // expected-error {{inout' before a parameter name is not allowed, place it before the parameter type instead}} {{13-18=}} {{28-28=inout }} 
// expected-error@-1 {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}} {{19-25=}}
func SR979c(let a: inout Int) {} // expected-error {{'let' as a parameter attribute is not allowed}} {{13-16=}}
func SR979d(let let a: Int) {}  // expected-error {{'let' as a parameter attribute is not allowed}} {{13-16=}} 
// expected-error @-1 {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}} {{17-21=}}
func SR979e(inout x: inout String) {} // expected-error {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}} {{13-18=}}
func SR979f(var inout x : Int) { // expected-error {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}} {{17-23=}}
// expected-error @-1 {{parameters may not have the 'var' specifier}} {{13-16=}}{{3-3=var x = x\n  }} 
  x += 10
}
func SR979g(inout i: inout Int) {} // expected-error {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}} {{13-18=}}
func SR979h(let inout x : Int) {}  // expected-error {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}} {{17-23=}}
// expected-error @-1 {{'let' as a parameter attribute is not allowed}}
class VarTester {
  init(var a: Int, var b: Int) {} // expected-error {{parameters may not have the 'var' specifier}} {{8-11=}} {{33-33= var a = a }}
  // expected-error @-1 {{parameters may not have the 'var' specifier}} {{20-24=}} {{33-33= var b = b }}
    func x(var b: Int) { //expected-error {{parameters may not have the 'var' specifier}} {{12-15=}} {{9-9=var b = b\n        }}
        b += 10
    }
}

func repeat() {}
// expected-error @-1 {{keyword 'repeat' cannot be used as an identifier here}}
// expected-note @-2 {{if this name is unavoidable, use backticks to escape it}} {{6-12=`repeat`}}

let for = 2
// expected-error @-1 {{keyword 'for' cannot be used as an identifier here}}
// expected-note @-2 {{if this name is unavoidable, use backticks to escape it}} {{5-8=`for`}}

func dog cow() {} // expected-error {{found an unexpected second identifier in function declaration; is there an accidental break?}}
// expected-note@-1 {{join the identifiers together}} {{6-13=dogcow}}
// expected-note@-2 {{join the identifiers together with camel-case}} {{6-13=dogCow}}
func cat Mouse() {} // expected-error {{found an unexpected second identifier in function declaration; is there an accidental break?}}
// expected-note@-1 {{join the identifiers together}} {{6-15=catMouse}}
func friend ship<T>(x: T) {} // expected-error {{found an unexpected second identifier in function declaration; is there an accidental break?}}
// expected-note@-1 {{join the identifiers together}} {{6-17=friendship}}
// expected-note@-2 {{join the identifiers together with camel-case}} {{6-17=friendShip}}
func were
wolf() {} // expected-error {{found an unexpected second identifier in function declaration; is there an accidental break?}}
// expected-note@-1 {{join the identifiers together}} {{6-5=werewolf}}
// expected-note@-2 {{join the identifiers together with camel-case}} {{6-5=wereWolf}}
func hammer
leavings<T>(x: T) {} // expected-error {{found an unexpected second identifier in function declaration; is there an accidental break?}}
// expected-note@-1 {{join the identifiers together}} {{6-9=hammerleavings}}
// expected-note@-2 {{join the identifiers together with camel-case}} {{6-9=hammerLeavings}}

prefix operator %
prefix func %<T>(x: T) -> T { return x } // No error expected - the < is considered an identifier but is peeled off by the parser.

struct Weak<T: class> { // expected-error {{'class' constraint can only appear on protocol declarations}}
  // expected-note@-1 {{did you mean to constrain 'T' with the 'AnyObject' protocol?}} {{16-21=AnyObject}}
  weak var value: T // expected-error {{'weak' may not be applied to non-class-bound 'T'; consider adding a protocol conformance that has a class bound}}
}

let x: () = ()
!() // expected-error {{missing argument for parameter #1 in call}}
!(()) // expected-error {{cannot convert value of type '()' to expected argument type 'Bool'}}
!(x) // expected-error {{cannot convert value of type '()' to expected argument type 'Bool'}}
!x // expected-error {{cannot convert value of type '()' to expected argument type 'Bool'}}
