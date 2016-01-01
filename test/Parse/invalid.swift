// RUN: %target-parse-verify-swift

func foo(a: Int) {
  // expected-error @+1 {{invalid character in source file}} {{8-9= }}
  foo(<\a\>) // expected-error {{invalid character in source file}} {{10-11= }}
  // expected-error @-1 {{'<' is not a prefix unary operator}}
  // expected-error @-2 {{'>' is not a postfix unary operator}}
}

// rdar://15946844
func test1(inout var x : Int) {}  // expected-error {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}} {{18-22=}}
func test2(inout let x : Int) {}  // expected-error {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}} {{18-22=}}

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

func test3(a: inout Int) {} // expected-error {{'inout' must appear before the parameter name}}{{12-12=inout }}{{15-21=}}

super.init() // expected-error {{'super' cannot be used outside of class members}} expected-error {{initializers may only be declared within a type}}

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
func d(b: String -> <T>() -> T) {} // expected-error {{expected type for function result}}
// expected-error @-1 {{expected ',' separator}} {{20-20=,}}
// expected-error @-2 {{expected parameter type following ':'}}
// expected-error @-3 {{expected ',' separator}}


// <rdar://problem/22143680> QoI: terrible diagnostic when trying to form a generic protocol
protocol Animal<Food> {  // expected-error {{protocols do not allow generic parameters; use associated types instead}}
  func feed(food: Food) // expected-error {{use of undeclared type 'Food'}}
}


