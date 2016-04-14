// RUN: %target-parse-verify-swift

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

func test3() {
  undeclared_func( // expected-error {{use of unresolved identifier 'undeclared_func'}} expected-note {{to match this opening '('}} expected-error {{expected ',' separator}} {{19-19=,}}
} // expected-error {{expected expression in list of expressions}} expected-error {{expected ')' in expression list}}

func runAction() {}

// rdar://16601779
func foo() {
  runAction(SKAction.sequence() // expected-error {{use of unresolved identifier 'SKAction'}}  expected-note {{to match this opening '('}} expected-error {{expected ',' separator}} {{32-32=,}}
    
    // expected-error @+2 {{expected ',' separator}} {{12-12=,}}
    // expected-error @+1 {{expected ',' separator}} {{12-12=,}}
    skview!
    // expected-error @-1 {{use of unresolved identifier 'skview'}}
} // expected-error {{expected expression in list of expressions}} expected-error {{expected ')' in expression list}}

super.init() // expected-error {{'super' cannot be used outside of class members}}

switch state { // expected-error {{use of unresolved identifier 'state'}}
  let duration : Int = 0 // expected-error {{all statements inside a switch must be covered by a 'case' or 'default'}} \
                         // expected-error {{expected expression}}
}

// rdar://18926814
func test4() {
  let abc = 123
  _ = " >> \( abc } ) << "   // expected-note {{to match this opening '('}}  expected-error {{expected ')' in expression list}}  expected-error {{expected ',' separator}} {{18-18=,}} expected-error {{expected ',' separator}} {{18-18=,}}  expected-error {{expected expression in list of expressions}}  expected-error {{extra tokens after interpolated string expression}}

}

// rdar://problem/18507467
func d(_ b: String -> <T>() -> T) {} // expected-error {{expected type for function result}}
// expected-error @-1 {{expected ',' separator}} {{22-22=,}}
// expected-error @-2 {{expected parameter name followed by ':'}}
// expected-error @-3 {{expected ',' separator}}


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
func SR979b(inout inout b: Int) {}  // expected-error {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}} {{19-25=}}
// expected-error @-1 {{'inout' before a parameter name is not allowed, place it before the parameter type instead}} {{13-18=}} {{28-28=inout }}
func SR979c(let a: inout Int) {}	 // expected-error {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}} {{13-16=}} 
// expected-error @-1 {{'let' as a parameter attribute is not allowed}} {{13-16=}}
func SR979d(let let a: Int) {}  // expected-error {{'let' as a parameter attribute is not allowed}} {{13-16=}} 
// expected-error @-1 {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}} {{17-21=}}
func SR979e(inout x: inout String) {} // expected-error {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}} {{13-18=}}
func SR979f(var inout x : Int) { // expected-error {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}} {{17-23=}}
// expected-error @-1 {{parameters may not have the 'var' specifier}} {{13-16=}}{{3-3=var x = x\n  }} 
  x += 10
}
func SR979h(let inout x : Int) {}  // expected-error {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}} {{17-23=}}
// expected-error @-1 {{'let' as a parameter attribute is not allowed}}
class VarTester {
  init(var a: Int, var b: Int) {} // expected-error {{parameters may not have the 'var' specifier}} {{8-11=}} {{33-33= var a = a }}
  // expected-error @-1 {{parameters may not have the 'var' specifier}} {{20-24=}} {{33-33= var b = b }}
    func x(var b: Int) { //expected-error {{parameters may not have the 'var' specifier}} {{12-15=}} {{9-9=var b = b\n        }}
        b += 10
    }
}
