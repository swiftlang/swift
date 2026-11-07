// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated

//===--- Helper types used in this file.

protocol FooProtocol {}

//===--- Tests.

func garbage() -> () {
  var a : Int
  ) this line is invalid, but we will stop at the keyword below... // expected-cparser-error{{expected expression}} expected-astgen-error {{unexpected code ') this line is invalid, but we will stop at the keyword below...' in function}}
  return a + "a" // expected-error{{binary operator '+' cannot be applied to operands of type 'Int' and 'String'}} expected-note {{overloads for '+' exist with these partially matching parameter lists: (Int, Int), (String, String)}}
  // expected-error@-1 {{no '+' candidates produce the expected contextual result type '()'}}
}

func moreGarbage() -> () {
  ) this line is invalid, but we will stop at the declaration... // expected-cparser-error{{expected expression}} expected-astgen-error {{unexpected code ') this line is invalid, but we will stop at the declaration...' in function}}
  func a() -> Int { return 4 }
  return a() + "a" // expected-error{{binary operator '+' cannot be applied to operands of type 'Int' and 'String'}} expected-note {{overloads for '+' exist with these partially matching parameter lists: (Int, Int), (String, String)}}
  // expected-error@-1 {{no '+' candidates produce the expected contextual result type '()'}}
}


class Container<T> {
  func exists() -> Bool { return true }
}

func useContainer() -> () {
  var a : Container<not a type [skip this greater: >] >, b : Int // expected-cparser-error{{expected '>' to complete generic argument list}} expected-astgen-error {{expected '>' to end generic argument clause}} expected-note{{to match this opening '<'}} expected-astgen-note {{'a' declared here}} expected-astgen-note {{did you mean 'a'?}} expected-astgen-error {{cannot find type 'not' in scope}} expected-astgen-error {{expected '=' in variable}} expected-astgen-error {{use of local variable 'a' before its declaration}} expected-astgen-error {{unexpected code 'this greater: >' in subscript}} expected-astgen-error {{expected expression after operator}}
  b = 5 // no-warning // // expected-astgen-error {{cannot find 'b' in scope}}
  a.exists()
}

@xyz class BadAttributes { // expected-error{{unknown attribute 'xyz'}}
  func exists() -> Bool { return true }
}

// expected-cparser-note @+2 {{did you mean 'test'?}}
// expected-note @+1 {{'test' declared here}}
func test(a: BadAttributes) -> () {
  _ = a.exists() // no-warning
}

// Here is an extra random close-brace!
} // expected-cparser-error{{extraneous '}' at top level}} {{1-3=}} expected-astgen-error {{unexpected brace in source file}}


//===--- Recovery for braced blocks.

func braceStmt1() {
  { braceStmt1(); } // expected-error {{closure expression is unused}} expected-note {{did you mean to use a 'do' statement?}} {{3-3=do }}
}

func braceStmt2() {
  { () in braceStmt2(); } // expected-error {{closure expression is unused}}
}

func braceStmt3() {
  {  // expected-error {{closure expression is unused}} expected-note {{did you mean to use a 'do' statement?}} {{3-3=do }}
    undefinedIdentifier {} // expected-error {{cannot find 'undefinedIdentifier' in scope}}
  }
}

//===--- Recovery for misplaced 'static'.

static func toplevelStaticFunc() {} // expected-error {{static methods may only be declared on a type}} {{1-8=}}

static struct StaticStruct {} // expected-cparser-error {{declaration cannot be marked 'static'}} {{1-8=}} expected-astgen-error {{static properties may only be declared on a type}}
static class StaticClass {} // expected-cparser-error {{declaration cannot be marked 'static'}} {{1-8=}} expected-astgen-error {{static properties may only be declared on a type}}
static protocol StaticProtocol {} // expected-cparser-error {{declaration cannot be marked 'static'}} {{1-8=}} expected-astgen-error {{static properties may only be declared on a type}}
static typealias StaticTypealias = Int // expected-cparser-error {{declaration cannot be marked 'static'}} {{1-8=}} expected-astgen-error {{static properties may only be declared on a type}}

class ClassWithStaticDecls {
  class var a = 42 // expected-error {{class stored properties not supported}}
}

//===--- Recovery for missing controlling expression in statements.

func missingControllingExprInIf() {
  if

  if { // expected-error {{missing condition in 'if' statement}}
  } // expected-cparser-error {{expected '{' after 'if' condition}} expected-astgen-error {{expected body in 'if' statement}}
  // expected-error@-2 {{cannot convert value of type 'Void' to expected condition type 'Bool'}}
  // expected-error@-3 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}
  // expected-error@-4 {{'if' must have an unconditional 'else' to be used as expression}}

  if // expected-error {{missing condition in 'if' statement}}
  {
  }

  if true {
  } else if { // expected-error {{missing condition in 'if' statement}}
  }

  // It is debatable if we should do recovery here and parse { true } as the
  // body, but the error message should be sensible.
  if { true } { // expected-error {{missing condition in 'if' statement}} expected-cparser-error{{consecutive statements on a line must be separated by ';'}} {{14-14=;}} expected-error {{closure expression is unused}} expected-note {{did you mean to use a 'do' statement?}} {{15-15=do }} expected-warning {{boolean literal is unused}}
  }

  if { true }() { // expected-error {{missing condition in 'if' statement}} expected-cparser-error 2 {{consecutive statements on a line must be separated by ';'}} expected-cparser-error {{closure expression is unused}} expected-astgen-error {{cannot call value of non-function type '()'}} expected-cparser-note {{did you mean to use a 'do' statement?}} {{17-17=do }} expected-warning {{boolean literal is unused}}
  }

  // <rdar://problem/18940198>
  if { { } } // expected-error{{missing condition in 'if' statement}} expected-error{{closure expression is unused}} expected-note {{did you mean to use a 'do' statement?}} {{8-8=do }}
}

func missingControllingExprInWhile() {
  while // expected-cparser-error {{expected expression, var, or let in 'while' condition}} expected-astgen-error {{expected expression and code block in 'while' statement}}

  while { // expected-error {{missing condition in 'while' statement}}
  }

  while // expected-error {{missing condition in 'while' statement}}
  {
  }

  // It is debatable if we should do recovery here and parse { true } as the
  // body, but the error message should be sensible.
  while { true } { // expected-error {{missing condition in 'while' statement}} expected-cparser-error{{consecutive statements on a line must be separated by ';'}} {{17-17=;}} expected-error {{closure expression is unused}} expected-note {{did you mean to use a 'do' statement?}} {{18-18=do }} expected-warning {{boolean literal is unused}}
  }

  while { true }() { // expected-error {{missing condition in 'while' statement}} expected-cparser-error 2 {{consecutive statements on a line must be separated by ';'}} expected-cparser-error {{closure expression is unused}} expected-astgen-error {{cannot call value of non-function type '()'}} expected-cparser-note {{did you mean to use a 'do' statement?}} {{20-20=do }} expected-warning {{boolean literal is unused}}
  }

  // <rdar://problem/18940198>
  while { { } } // expected-error{{missing condition in 'while' statement}} expected-error{{closure expression is unused}} expected-note {{did you mean to use a 'do' statement?}} {{11-11=do }}
}

func missingControllingExprInRepeatWhile() {
  repeat {
  } while // expected-error {{missing condition in 'while' statement}}
  { // expected-cparser-error{{closure expression is unused}} expected-astgen-error {{function produces expected type 'Bool'; did you mean to call it with '()'?}} expected-cparser-note {{did you mean to use a 'do' statement?}} {{3-3=do }}
    missingControllingExprInRepeatWhile(); // // expected-astgen-error {{cannot convert value of type '()' to closure result type 'Bool'}}
  }

  repeat {
  } while { true }() // expected-error{{missing condition in 'while' statement}} expected-cparser-error{{consecutive statements on a line must be separated by ';'}} {{10-10=;}} expected-cparser-warning {{result of call to closure returning 'Bool' is unused}}
}

// https://github.com/apple/swift/issues/42787
func missingWhileInRepeat() {
  repeat {
  } // expected-cparser-error {{expected 'while' after body of 'repeat' statement}} expected-astgen-error {{expected 'while' and condition in 'repeat' statement}}
}

func acceptsClosure<T>(t: T) -> Bool { return true }

func missingControllingExprInFor() {
  for ; { // expected-cparser-error {{C-style for statement was removed in Swift 3}} expected-astgen-error {{expected pattern, 'in', and expression in 'for' statement}} expected-astgen-error {{unexpected ';' separator}}
  }

  for ; // expected-cparser-error {{C-style for statement was removed in Swift 3}} expected-astgen-error {{expected pattern, 'in', and expression in 'for' statement}} expected-astgen-error {{unexpected ';' separator}}
  { 
  }

  for ; true { // expected-cparser-error {{C-style for statement was removed in Swift 3}} expected-astgen-error {{expected pattern, 'in', and expression in 'for' statement}} expected-astgen-error {{unexpected code '; true' in 'for' statement}}
  }

  for var i = 0; true { // expected-cparser-error {{C-style for statement was removed in Swift 3}} expected-astgen-error {{expected 'in' and expression in 'for' statement}} expected-astgen-error {{unexpected code '= 0; true' in 'for' statement}}
    i += 1
  }
}

func missingControllingExprInForEach() {
  // expected-error @+3 {{expected pattern}}
  // expected-cparser-error @+2 {{expected Sequence expression for for-each loop}}
  // expected-cparser-error @+1 {{expected '{' to start the body of for-each loop}}
  for

  // expected-error @+2 {{expected pattern}}
  // expected-cparser-error @+1 {{expected Sequence expression for for-each loop}}
  for {
  }

  // expected-error @+2 {{expected pattern}}
  // expected-cparser-error @+1 {{expected Sequence expression for for-each loop}}
  for
  {
  }

  // expected-cparser-error @+2 {{expected 'in' after for-each pattern}}
  // expected-cparser-error @+1 {{expected Sequence expression for for-each loop}} expected-astgen-error@+1 {{expected 'in' and expression in 'for' statement}}
  for i {
  }

  // expected-cparser-error @+2 {{expected 'in' after for-each pattern}}
  // expected-cparser-error @+1 {{expected Sequence expression for for-each loop}} expected-astgen-error@+1 {{expected 'in' and expression in 'for' statement}}
  for var i {
  }

  // expected-cparser-error @+2 {{expected pattern}} expected-astgen-error@+2 {{expected 'in' and expression in 'for' statement}}
  // expected-cparser-error @+1 {{expected Sequence expression for for-each loop}} expected-astgen-error@+1 {{keyword 'in' cannot be used as an identifier here}}
  for in {
  }

  // expected-error @+1 {{expected pattern}}
  for 0..<12 {
  }

  // expected-cparser-error @+3 {{expected pattern}}
  // expected-error @+2 {{expected Sequence expression for for-each loop}}
  // expected-cparser-error @+1 {{expected '{' to start the body of for-each loop}} expected-astgen-error@+1 {{keyword 'for' cannot be used as an identifier here}}
  for for in {
  }

  for i in { // expected-error {{expected Sequence expression for for-each loop}}
  }

// The #if block is used to provide a scope for the for stmt to force it to end
// where necessary to provoke the crash.
#if true  // <rdar://problem/21679557> compiler crashes on "for{{"
  // expected-error @+4 {{expected pattern}}
  // expected-cparser-error @+3 {{expected Sequence expression for for-each loop}} expected-astgen-error@+3 {{expected '}' to end closure}}
  // expected-error @+2 {{closure expression is unused}}
  // expected-note @+1 {{did you mean to use a 'do' statement?}}
  for{{ // expected-note 2 {{to match this opening '{'}} expected-astgen-error {{expected '}' to end 'for' statement}}
#endif  // expected-cparser-error {{expected '}' at end of closure}} expected-cparser-error {{expected '}' at end of brace statement}}

#if true
  // expected-error @+2 {{expected pattern}}
  // expected-cparser-error @+1 {{expected Sequence expression for for-each loop}}
  for{
    var x = 42
  }
#endif
  
  // https://github.com/apple/swift/issues/48502
  struct User { let name: String? }
  let users = [User]()
  for user in users whe { // expected-cparser-error {{expected '{' to start the body of for-each loop}} expected-astgen-error {{unexpected code 'whe' in 'for' statement}}
    if let name = user.name {
      let key = "\(name)"
    }
  }

  for // expected-error {{expected pattern}} expected-cparser-error {{Sequence expression for for-each loop}}
  ; // expected-cparser-error {{expected '{' to start the body of for-each loop}}
}

// 'missingControllingExprInSwitch' is in recovery_switch.swift: SwiftParser
// nests the 'switch' statements after the first, which hides the diagnostics
// of the rest of this file.

//===--- Recovery for missing braces in nominal type decls.

struct NoBracesStruct1() // expected-cparser-error {{expected '{' in struct}} expected-astgen-error {{expected member block in struct}}
enum NoBracesUnion1() // expected-cparser-error {{expected '{' in enum}} expected-astgen-error {{expected member block in enum}}
class NoBracesClass1() // expected-cparser-error {{expected '{' in class}} expected-astgen-error {{expected member block in class}}
protocol NoBracesProtocol1() // expected-cparser-error {{expected '{' in protocol type}} expected-astgen-error {{expected member block in protocol}}
extension NoBracesStruct1() // expected-cparser-error {{expected '{' in extension}} expected-astgen-error {{expected member block in extension}}

struct NoBracesStruct2 // expected-cparser-error {{expected '{' in struct}} expected-astgen-error {{expected member block in struct}}
enum NoBracesUnion2 // expected-cparser-error {{expected '{' in enum}} expected-astgen-error {{expected member block in enum}}
class NoBracesClass2 // expected-cparser-error {{expected '{' in class}} expected-astgen-error {{expected member block in class}}
protocol NoBracesProtocol2 // expected-cparser-error {{expected '{' in protocol type}} expected-astgen-error {{expected member block in protocol}}
extension NoBracesStruct2 // expected-cparser-error {{expected '{' in extension}} expected-astgen-error {{expected member block in extension}}

//===--- Recovery for multiple identifiers in decls

protocol Multi ident {}
// expected-cparser-error @-1 {{found an unexpected second identifier in protocol declaration; is there an accidental break?}} expected-astgen-error@-1 {{found an unexpected second identifier in protocol; is there an accidental break?}}
// expected-cparser-note @-2 {{join the identifiers together}} {{10-21=Multiident}}
// expected-cparser-note @-3 {{join the identifiers together with camel-case}} {{10-21=MultiIdent}}

class CCC CCC<T> {}
// expected-cparser-error @-1 {{found an unexpected second identifier in class declaration; is there an accidental break?}} expected-astgen-error@-1 {{found an unexpected second identifier in class; is there an accidental break?}}
// expected-cparser-note @-2 {{join the identifiers together}} {{7-14=CCCCCC}}

enum EE EE<T> where T : Multi {
// expected-cparser-error @-1 {{found an unexpected second identifier in enum declaration; is there an accidental break?}} expected-astgen-error@-1 {{found an unexpected second identifier in enum; is there an accidental break?}} 
// expected-cparser-note @-2 {{join the identifiers together}} {{6-11=EEEE}}
  
  case a a
  // expected-cparser-error @-1 {{found an unexpected second identifier in enum 'case' declaration; is there an accidental break?}} expected-astgen-error@-1 {{unexpected code 'a' in enum}} 
  // expected-cparser-note @-2 {{join the identifiers together}} {{8-11=aa}}
  // expected-cparser-note @-3 {{join the identifiers together with camel-case}} {{8-11=aA}}
  
  case b
}

struct SS SS : Multi {
// expected-cparser-error @-1 {{found an unexpected second identifier in struct declaration; is there an accidental break?}} expected-astgen-error@-1 {{found an unexpected second identifier in struct; is there an accidental break?}}
// expected-cparser-note @-2 {{join the identifiers together}} {{8-13=SSSS}}
  
  private var a b : Int = ""
  // expected-cparser-error @-1 {{found an unexpected second identifier in variable declaration; is there an accidental break?}} expected-astgen-error@-1 {{found an unexpected second identifier in pattern; is there an accidental break?}}
  // expected-cparser-note @-2 {{join the identifiers together}} {{15-18=ab}}
  // expected-cparser-note @-3 {{join the identifiers together with camel-case}} {{15-18=aB}}
  // expected-error @-4 {{cannot convert value of type 'String' to specified type 'Int'}}
  
  func f() {
    var c d = 5 // // expected-astgen-error {{cannot find type 'd' in scope}}
    // expected-cparser-error @-1 {{found an unexpected second identifier in variable declaration; is there an accidental break?}} expected-astgen-error@-1 {{expected ':' in type annotation}}
    // expected-cparser-note @-2 {{join the identifiers together}} {{9-12=cd}}
    // expected-cparser-note @-3 {{join the identifiers together with camel-case}} {{9-12=cD}}
    // expected-cparser-warning @-4 {{initialization of variable 'c' was never used; consider replacing with assignment to '_' or removing it}}
    
    let _ = 0
  }
}

let (efg hij, foobar) = (5, 6) // // expected-astgen-error {{'(Int, Int)' is not convertible to '(Int, Int, _)', tuples have a different number of elements}}
// expected-cparser-error @-1 {{found an unexpected second identifier in constant declaration; is there an accidental break?}} expected-astgen-error@-1 {{expected ',' in tuple pattern}}
// expected-cparser-note @-2 {{join the identifiers together}} {{6-13=efghij}}
// expected-cparser-note @-3 {{join the identifiers together with camel-case}} {{6-13=efgHij}}

_ = foobar // OK. //


//===--- Recovery for parse errors in types.

struct ErrorTypeInVarDecl1 {
  var v1 : // expected-cparser-error {{expected type}} {{11-11= <#type#>}} expected-astgen-error {{expected type}}{{136-136= <#type#>}}
}

struct ErrorTypeInVarDecl2 {
  var v1 : Int. // expected-cparser-error {{expected member name following '.'}} expected-astgen-error {{expected name in member type}} expected-astgen-error {{'' is not a member type of struct 'Swift.Int'}}
  var v2 : Int
}

struct ErrorTypeInVarDecl3 {
  var v1 : Int< // expected-cparser-error {{expected type}} expected-astgen-error {{expected '>' to end generic argument clause}} expected-astgen-error {{cannot specialize non-generic type 'Int'}} expected-astgen-note {{to match this opening '<'}}
  var v2 : Int
}

struct ErrorTypeInVarDecl4 {
  var v1 : Int<, // expected-cparser-error {{expected type}} {{16-16= <#type#>}} expected-astgen-error {{expected '>' to end generic argument clause}} expected-astgen-error {{cannot specialize non-generic type 'Int'}} expected-astgen-note {{to match this opening '<'}}
  var v2 : Int // // expected-astgen-error {{'var' cannot appear nested inside another 'var' or 'let' pattern}}
}

struct ErrorTypeInVarDecl5 {
  var v1 : Int<Int // expected-cparser-error {{expected '>' to complete generic argument list}} expected-astgen-error {{expected '>' to end generic argument clause}} expected-note {{to match this opening '<'}} expected-astgen-error {{cannot specialize non-generic type 'Int'}}
  var v2 : Int
}

struct ErrorTypeInVarDecl6 {
  var v1 : Int<Int, // expected-note {{to match this opening '<'}} expected-astgen-error {{cannot specialize non-generic type 'Int'}}
               Int // expected-cparser-error {{expected '>' to complete generic argument list}} expected-astgen-error {{expected '>' to end generic argument clause}}
  var v2 : Int
}


struct ErrorTypeInVarDecl7 {
  var v1 : Int<Int, // expected-error {{expected type}} expected-astgen-error {{cannot specialize non-generic type 'Int'}} expected-astgen-note {{to match this opening '<'}} expected-astgen-error {{expected '>' to end generic argument clause}}
  var v2 : Int
}

struct ErrorTypeInVarDecl8 {
  var v1 : protocol<FooProtocol // expected-cparser-error {{expected '>' to complete protocol-constrained type}} expected-astgen-error {{'protocol<...>' composition syntax has been removed and is not needed here}} expected-note {{to match this opening '<'}} expected-astgen-error {{expected '>' to end generic argument clause}}
  var v2 : Int
}

struct ErrorTypeInVarDecl9 {
  var v1 : protocol // expected-error {{expected type}} expected-astgen-error {{expected identifier and member block in protocol}}
  var v2 : Int
}

struct ErrorTypeInVarDecl10 {
  var v1 : protocol<FooProtocol // expected-cparser-error {{expected '>' to complete protocol-constrained type}} expected-astgen-error {{'protocol<...>' composition syntax has been removed and is not needed here}} expected-note {{to match this opening '<'}} expected-astgen-error {{expected '>' to end generic argument clause}}
  var v2 : Int
}

struct ErrorTypeInVarDecl11 {
  var v1 : protocol<FooProtocol, // expected-error {{expected type}} expected-astgen-error {{'protocol<...>' composition syntax has been removed and is not needed here}} expected-astgen-note {{to match this opening '<'}} expected-astgen-error {{expected '>' to end generic argument clause}}
  var v2 : Int
}

func ErrorTypeInPattern1(_: protocol<) { } // expected-cparser-error {{expected type}} expected-astgen-error {{'protocol<>' syntax has been removed; use 'Any' instead}} expected-astgen-note {{to match this opening '<'}} expected-astgen-error {{expected '>' to end generic argument clause}}
func ErrorTypeInPattern2(_: protocol<F) { } // expected-cparser-error {{expected '>' to complete protocol-constrained type}} expected-astgen-error {{'protocol<...>' composition syntax has been removed and is not needed here}} expected-astgen-error {{expected '>' to end generic argument clause}}
                                            // expected-note@-1 {{to match this opening '<'}}
                                            // expected-error@-2 {{cannot find type 'F' in scope}}

func ErrorTypeInPattern3(_: protocol<F,) { } // expected-error {{expected type}} expected-astgen-error {{'protocol<...>' composition syntax has been removed and is not needed here}} expected-astgen-note {{to match this opening '<'}} expected-astgen-error {{expected '>' to end generic argument clause}}
                                             // expected-error@-1 {{cannot find type 'F' in scope}}

struct ErrorTypeInVarDecl12 {
  var v1 : FooProtocol & // expected-cparser-error{{expected identifier for type name}} expected-astgen-error {{expected type in type composition}}
  var v2 : Int
}

struct ErrorTypeInVarDecl13 { // expected-cparser-note {{in declaration of 'ErrorTypeInVarDecl13'}}
  var v1 : & FooProtocol // expected-error {{expected type}} expected-cparser-error {{consecutive declarations on a line must be separated by ';'}} expected-cparser-error{{expected declaration}} 
  var v2 : Int
}

struct ErrorTypeInVarDecl16 {
  var v1 : FooProtocol & // expected-cparser-error {{expected identifier for type name}} expected-astgen-error {{expected type in type composition}}
  var v2 : Int
}

func ErrorTypeInPattern4(_: FooProtocol & ) { } // expected-cparser-error {{expected identifier for type name}} expected-astgen-error {{expected type in type composition}}


struct ErrorGenericParameterList1< // expected-cparser-error {{expected an identifier to name generic parameter}} expected-cparser-error {{expected '{' in struct}} expected-astgen-error {{expected '>' to end generic parameter clause}} expected-astgen-error {{expected member block in struct}} expected-astgen-note {{to match this opening '<'}}

struct ErrorGenericParameterList2<T // expected-cparser-error {{expected '>' to complete generic parameter list}} expected-astgen-error {{expected member block in struct}} expected-note {{to match this opening '<'}} expected-cparser-error {{expected '{' in struct}} expected-astgen-error {{expected '>' to end generic parameter clause}}

struct ErrorGenericParameterList3<T, // expected-cparser-error {{expected an identifier to name generic parameter}} expected-astgen-error {{expected '>' to end generic parameter clause}} expected-cparser-error {{expected '{' in struct}} expected-astgen-error {{expected generic parameter in generic parameter clause}} expected-astgen-note {{to match this opening '<'}} expected-astgen-error {{expected member block in struct}}

// Note: Don't move braces to a different line here.
struct ErrorGenericParameterList4< // expected-cparser-error {{expected an identifier to name generic parameter}} expected-astgen-error {{expected '>' to end generic parameter clause}} expected-astgen-note {{to match this opening '<'}}
{
}

// Note: Don't move braces to a different line here.
struct ErrorGenericParameterList5<T // expected-cparser-error {{expected '>' to complete generic parameter list}} expected-astgen-error {{expected '>' to end generic parameter clause}} expected-note {{to match this opening '<'}}
{
}

// Note: Don't move braces to a different line here.
struct ErrorGenericParameterList6<T, // expected-cparser-error {{expected an identifier to name generic parameter}} expected-astgen-error {{expected generic parameter in generic parameter clause}} expected-astgen-note {{to match this opening '<'}} expected-astgen-error {{expected '>' to end generic parameter clause}}
{
}

struct ErrorTypeInVarDeclFunctionType1 {
  var v1 : () -> // expected-cparser-error {{expected type for function result}} expected-astgen-error {{expected return type in function type}}
  var v2 : Int
}

struct ErrorTypeInVarDeclArrayType1 {
  var v1 : Int[+] // expected-cparser-error {{array types are now written with the brackets around the element type}} expected-astgen-error {{cannot convert value of type '[Any]' to specified type 'Int'}}
  // expected-cparser-error @-1 {{expected expression after unary operator}} expected-astgen-error@-1 {{unexpected code '+' in array}}
  // expected-cparser-error @-2 {{expected expression}} expected-astgen-error@-2 {{expected '=' in variable}}
  var v2 : Int
}

struct ErrorTypeInVarDeclArrayType2 {
  var v1 : Int[+ // expected-cparser-error {{unary operator cannot be separated from its operand}} expected-astgen-error {{expected ']' to end array}} expected-astgen-error {{expected expression after operator}}
                 // expected-cparser-error@-1 {{expected ']' in array type}} expected-astgen-error@-1 {{expected '=' in variable}}
                 // expected-note@-2 {{to match this opening '['}}
  var v2 : Int // expected-cparser-error {{expected expression}}
}

struct ErrorTypeInVarDeclArrayType3 {
  var v1 : Int[ // expected-cparser-error {{expected ']' in array type}} expected-astgen-error {{expected '=' in variable}} expected-astgen-error {{cannot convert value of type '[Any]' }} expected-astgen-error {{expected ']' to end array}}
                // expected-note@-1 {{to match this opening '['}}
  ;  // expected-cparser-error {{expected expression}}
  var v2 : Int
}

struct ErrorTypeInVarDeclArrayType4 {
  var v1 : Int[1 // expected-cparser-error {{expected ']' in array type}} expected-astgen-error {{expected '=' in variable}} expected-note {{to match this opening '['}} expected-astgen-error {{cannot convert value of type '[Int]' to specified type 'Int'}} expected-astgen-error {{expected ']' to end array}}
  var v2 : Int] // expected-error {{unexpected ']' in type; did you mean to write an array type?}} {{12-12=[}}

}

struct ErrorTypeInVarDeclArrayType5 { // expected-cparser-note {{in declaration of 'ErrorTypeInVarDeclArrayType5'}}
  let a1: Swift.Int] // expected-error {{unexpected ']' in type; did you mean to write an array type?}} {{11-11=[}}
  let a2: Set<Int]> // expected-cparser-error {{expected '>' to complete generic argument list}} expected-astgen-error {{expected '>' to end generic argument clause}} // expected-note {{to match this opening '<'}} expected-astgen-error {{unexpected code ']>' in struct}}
  let a3: Set<Int>] // expected-error {{unexpected ']' in type; did you mean to write an array type?}} {{11-11=[}}
  let a4: Int]? // expected-error {{unexpected ']' in type; did you mean to write an array type?}} {{11-11=[}}
  // expected-cparser-error @-1 {{consecutive declarations on a line must be separated by ';'}} // expected-cparser-error @-1 {{expected declaration}}
  let a5: Int?] // expected-error {{unexpected ']' in type; did you mean to write an array type?}} {{11-11=[}}
  let a6: [Int]] // expected-error {{unexpected ']' in type; did you mean to write an array type?}} {{11-11=[}}
  let a7: [String: Int]] // expected-error {{unexpected ']' in type; did you mean to write an array type?}} {{11-11=[}}
}

struct ErrorTypeInVarDeclDictionaryType {
  let a1: String: // expected-cparser-error {{unexpected ':' in type; did you mean to write a dictionary type?}} {{11-11=[}}
  // expected-cparser-error @-1 {{expected dictionary value type}} expected-astgen-error@-1 {{unexpected code ':' in struct}}
  let a2: String: Int] // expected-cparser-error {{unexpected ':' in type; did you mean to write a dictionary type?}} {{11-11=[}} expected-astgen-error {{expected '[' to start dictionary type}}
  let a3: String: [Int] // expected-cparser-error {{unexpected ':' in type; did you mean to write a dictionary type?}} {{11-11=[}} {{24-24=]}} expected-astgen-error {{expected '[' to start dictionary type}} expected-astgen-error {{expected ']' to end dictionary type}}
  let a4: String: Int // expected-cparser-error {{unexpected ':' in type; did you mean to write a dictionary type?}} {{11-11=[}} {{22-22=]}} expected-astgen-error {{expected '[' to start dictionary type}} expected-astgen-error {{expected ']' to end dictionary type}}
}

struct ErrorInFunctionSignatureResultArrayType1 {
  func foo() -> Int[ { // expected-cparser-error {{expected '{' in body of function declaration}} expected-astgen-error {{unexpected code '[' in function}}
                       // expected-cparser-note@-1 {{to match this opening '['}}
    return [0] // // expected-astgen-error {{cannot convert return expression of type '[Int]' to return type 'Int'}}
  }  // expected-cparser-error {{expected ']' in array type}}
  func bar() -> Int] { // expected-error {{unexpected ']' in type; did you mean to write an array type?}} {{17-17=[}}
    return [0]
  }
}

struct ErrorInFunctionSignatureResultArrayType2 {
  func foo() -> Int[0 { // expected-cparser-error {{expected ']' in array type}} expected-astgen-error {{unexpected code '[0' in function}} expected-cparser-note {{to match this opening '['}}
    return [0]  // expected-error {{cannot convert return expression of type '[Int]' to return type 'Int'}}
  }
}

struct ErrorInFunctionSignatureResultArrayType3 {
  func foo() -> Int[0] { // expected-cparser-error {{array types are now written with the brackets around the element type}} {{17-17=[}} {{20-21=}} expected-astgen-error {{}}
    return [0] // // expected-astgen-error {{cannot convert return expression of type '[Int]' to return type 'Int'}}
  }
}

struct ErrorInFunctionSignatureResultArrayType4 {
  func foo() -> Int[0_1] { // expected-cparser-error {{array types are now written with the brackets around the element type}} {{17-17=[}} {{20-21=}} expected-astgen-error {{unexpected code '[0_1]' in function}}
    return [0] // // expected-astgen-error {{cannot convert return expression of type '[Int]' to return type 'Int'}}
  }
}


struct ErrorInFunctionSignatureResultArrayType5 {
  func foo() -> Int[0b1] { // expected-cparser-error {{array types are now written with the brackets around the element type}} {{17-17=[}} {{20-21=}} expected-astgen-error {{unexpected code '[0b1]' in function}}
    return [0] // // expected-astgen-error {{cannot convert return expression of type '[Int]' to return type 'Int'}}
  }
}


struct ErrorInFunctionSignatureResultArrayType11 { // expected-cparser-note{{in declaration of 'ErrorInFunctionSignatureResultArrayType11'}}
  func foo() -> Int[(a){a++}] { // expected-cparser-error {{consecutive declarations on a line must be separated by ';'}} {{29-29=;}} expected-cparser-error {{expected ']' in array type}} expected-cparser-note {{to match this opening '['}} expected-cparser-error {{cannot find operator '++' in scope; did you mean '+= 1'?}} expected-cparser-error {{cannot find 'a' in scope}} expected-cparser-error {{expected declaration}} expected-astgen-error {{unexpected code '[(a){a++}]' in function}}
  }
}

//===--- Recovery for missing initial value in var decls.

struct MissingInitializer1 {
  var v1 : Int = // expected-cparser-error {{expected initial value after '='}} expected-astgen-error {{expected expression in variable}}
}

//===--- Recovery for expr-postfix.

func exprPostfix1(x : Int) {
  x. // expected-cparser-error {{expected member name following '.'}} expected-astgen-error {{expected name in member access}}
}

func exprPostfix2() {
  _ = .42 // expected-cparser-error {{'.42' is not a valid floating point literal; it must be written '0.42'}} {{7-7=0}} expected-astgen-error {{'.42' is not a valid floating point literal; it must be written '0.42'}}
}

//===--- Recovery for expr-super.

class ExprSuper {
  init() {
    super. // expected-cparser-error {{expected member name following '.'}}
    // expected-cparser-error@-1 {{'super' cannot be used in class 'ExprSuper' because it has no superclass}} expected-astgen-error@-1 {{expected name in member access}}
  }
}

//===--- Recovery for braces inside a nominal decl.

struct BracesInsideNominalDecl1 { // expected-cparser-note{{in declaration of 'BracesInsideNominalDecl1'}}
  { // expected-cparser-error {{expected declaration}} expected-astgen-error {{unexpected code in struct}}
    aaa
  }
  typealias A = Int
}
func use_BracesInsideNominalDecl1() {
  // Ensure that the typealias decl is not skipped.
  var _ : BracesInsideNominalDecl1.A // no-error
}

// https://github.com/apple/swift/issues/43383
class С_43383 {
    print("No one else was in the room where it happened") // expected-cparser-note {{'print()' previously declared here}}
    // expected-cparser-error @-1 {{expected 'func' keyword in instance method declaration}}
    // expected-cparser-error @-2 {{expected '{' in body of function declaration}} expected-astgen-error@-2 {{unexpected code '"No one else was in the room where it happened"' in parameter clause}}
    // expected-cparser-error @-3 {{expected parameter name followed by ':'}} expected-astgen-error@-3 {{expected 'func' in function}}
}
extension С_43383 { //
    print("The room where it happened, the room where it happened")
    // expected-cparser-error @-1 {{expected 'func' keyword in instance method declaration}}
    // expected-cparser-error @-2 {{invalid redeclaration of 'print()'}} expected-astgen-error@-2 {{unexpected code '"The room where it happened, the room where it happened"' in parameter clause}}
    // expected-cparser-error @-3 {{expected parameter name followed by ':'}} expected-astgen-error@-3 {{expected 'func' in function}}
}


//===--- Recovery for wrong decl introducer keyword.

class WrongDeclIntroducerKeyword1 {
  notAKeyword() {} // expected-cparser-error {{expected 'func' keyword in instance method declaration}} expected-astgen-error {{expected 'func' in function}}
  func foo() {}
  class func bar() {}
}

//===--- Recovery for wrong inheritance clause.

class Base2<T> {
}

class SubModule {
    class Base1 {}
    class Base2<T> {}
}

// expected-cparser-error@+1 {{expected ':' to begin inheritance clause}} {{30-31=: }} {{34-35=}} expected-astgen-error@+1 {{expected ':' to begin inheritance clause}}{{31-31=: }}{{30-31=}}{{34-36=}}{{34-34= }}    
class WrongInheritanceClause1(Int) {} // // expected-astgen-error {{inheritance from non-protocol, non-class type 'Int'}}

// expected-cparser-error@+1 {{expected ':' to begin inheritance clause}} {{30-31=: }} {{41-42=}} expected-astgen-error@+1 {{expected ':' to begin inheritance clause}}{{31-31=: }}{{30-31=}}{{41-43=}}
class WrongInheritanceClause2(Base2<Int>) {}

// expected-cparser-error@+1 {{expected ':' to begin inheritance clause}} {{33-34=: }} {{49-50=}} expected-astgen-error@+1 {{expected ':' to begin inheritance clause}}{{34-34=: }}{{33-34=}}{{49-51=}}{{49-49= }}
class WrongInheritanceClause3<T>(SubModule.Base1) where T:AnyObject {}

// expected-cparser-error@+1 {{expected ':' to begin inheritance clause}} {{30-31=: }} {{51-52=}} expected-astgen-error@+1 {{expected ':' to begin inheritance clause}}{{31-31=: }}{{30-31=}}{{51-53=}}
class WrongInheritanceClause4(SubModule.Base2<Int>) {}

// expected-cparser-error@+1 {{expected ':' to begin inheritance clause}} {{33-34=: }} {{54-55=}} expected-astgen-error@+1 {{expected ':' to begin inheritance clause}}{{34-34=: }}{{33-34=}}{{54-56=}}
class WrongInheritanceClause5<T>(SubModule.Base2<Int>) where T:AnyObject {}

// expected-cparser-error@+1 {{expected ':' to begin inheritance clause}} {{30-31=: }} expected-astgen-error@+1 {{expected ':' to begin inheritance clause}}{{31-31=: }}{{30-31=}} 
class WrongInheritanceClause6(Int {} // // expected-astgen-error {{inheritance from non-protocol, non-class type 'Int'}}

// expected-cparser-error@+1 {{expected ':' to begin inheritance clause}} {{33-34=: }} expected-astgen-error@+1 {{expected ':' to begin inheritance clause}}{{34-34=: }}{{33-34=}} 
class WrongInheritanceClause7<T>(Int where T:AnyObject {} // // expected-astgen-error {{inheritance from non-protocol, non-class type 'Int'}}

class Base {}

// <rdar://problem/18502220> [swift-crashes 078] parser crash on invalid cast in sequence expr
Base=1 as Base=1 // expected-error{{cannot convert value of type 'Int' to type 'Base' in coercion}}
// expected-error@-1 {{cannot assign to immutable expression of type 'Base.Type'}}
// expected-error@-2 {{cannot assign to immutable expression of type 'Base'}}
// expected-error@-3 {{cannot assign value of type '()' to type 'Base.Type'}}
// expected-error@-4 {{cannot assign value of type 'Int' to type 'Base'}}

// <rdar://problem/18634543> Parser hangs at swift::Parser::parseType
public enum TestA {
  // expected-cparser-error @+1{{expected '{' in body of function declaration}}
  public static func convertFromExtenndition( // expected-cparser-error {{expected parameter name followed by ':'}}
    // expected-cparser-error@+1{{expected parameter name followed by ':'}} expected-astgen-error@+1 {{expected ':' and type in parameter}}
    s._core.count != 0, "Can't form a Character from an empty String") // // expected-astgen-error {{unexpected code '._core.count != 0, "Can't form a Character from an empty String"' in parameter clause}}
}

public enum TestB {
  // expected-cparser-error@+1{{expected '{' in body of function declaration}}
  public static func convertFromExtenndition( // expected-cparser-error {{expected parameter name followed by ':'}}
    // expected-cparser-error@+1 {{expected parameter name followed by ':'}} expected-astgen-error@+1 {{expected ':' and type in parameter}}
    s._core.count ?= 0, "Can't form a Character from an empty String") // // expected-astgen-error {{unexpected code '._core.count ?= 0, "Can't form a Character from an empty String"' in parameter clause}}
}



// <rdar://problem/18634543> Infinite loop and unbounded memory consumption in parser
class bar {}
var baz: bar
// expected-cparser-error@+1{{unnamed parameters must be written with the empty name '_'}} expected-astgen-error@+1 {{expected ':' and type in parameter}}
func foo1(bar!=baz) {} // expected-note {{did you mean 'foo1'?}} expected-astgen-error {{unexpected code '!=baz' in parameter clause}}
// expected-cparser-error@+1{{unnamed parameters must be written with the empty name '_'}} expected-astgen-error@+1 {{expected ':' and type in parameter}}
func foo2(bar! = baz) {}// expected-note {{did you mean 'foo2'?}} expected-astgen-error {{unexpected code '! = baz' in parameter clause}}

// rdar://19605567
// expected-error@+1{{cannot find 'esp' in scope; did you mean 'test'?}}
switch esp {
case let (jeb):
  // expected-cparser-error@+2{{expected an identifier to name generic parameter}} expected-astgen-error@+2 {{expected member block in class}}
  // expected-cparser-error@+1{{expected '{' in class}} expected-astgen-error@+1 {{expected '>' to end generic parameter clause}}
  class Ceac<}> {} // expected-astgen-note {{to match this opening '<'}} expected-astgen-error {{unexpected code in source file}}
// expected-cparser-error@+1{{extraneous '}' at top level}} {{1-+1:1=}}
}


#if true

// rdar://19605164
// expected-error@+2{{cannot find type 'S' in scope}}
struct Foo19605164 {
func a(s: S[{{g) -> Int {} // expected-cparser-note {{to match this opening '['}} expected-astgen-note {{to match this opening '('}} expected-astgen-error {{closure expression is unused}} expected-astgen-error {{cannot infer type of closure parameter 'g' without a type annotation}} expected-astgen-error {{expected 'in' in closure signature}} expected-astgen-error {{unexpected code ') -> Int {}' in closure}}
}}} // expected-cparser-error {{expected ']' in array type}}
// expected-cparser-error@-2{{consecutive statements on a line must be separated by ';'}} expected-astgen-error@-2 {{unexpected code '[' in function}}
// expected-cparser-error@-3{{expected expression}} expected-astgen-error@-3 {{expected ')' to end parameter clause}}
#endif
  
// rdar://19605567
// expected-cparser-error@+6{{expected '(' for initializer parameters}}
// expected-error@+5{{initializers may only be declared within a type}}
// expected-cparser-error@+4{{expected an identifier to name generic parameter}}
// expected-cparser-error@+3{{consecutive statements on a line must be separated by ';'}} expected-astgen-error@+3 {{unexpected code ')}' in source file}}
// expected-cparser-error@+2{{expected expression}} expected-astgen-error@+2 {{expected ')' to end parameter clause}}
// expected-cparser-error@+1{{extraneous '}' at top level}} expected-astgen-error@+1 {{expected '>' to end generic parameter clause}}
func F() { init<( } )} // expected-cparser-note 2{{did you mean 'F'?}} expected-astgen-note {{did you mean 'F'?}} expected-astgen-note {{to match this opening '<'}} expected-astgen-note {{to match this opening '('}}

struct InitializerWithName {
  init x() {} // expected-error {{initializers cannot have a name}} {{8-9=}}
}

struct InitializerWithNameAndParam {
  init a(b: Int) {} // expected-error {{initializers cannot have a name}} {{8-9=}}
  init? c(_ d: Int) {} // expected-error {{initializers cannot have a name}} {{9-10=}}
  init e<T>(f: T) {} // expected-cparser-error {{initializers cannot have a name}} {{8-9=}} expected-astgen-error {{initializers cannot have a name}}{{8-12=}}{{7-8=}} expected-astgen-error {{cannot find type 'T' in scope}}
  init? g<T>(_: T) {} // expected-cparser-error {{initializers cannot have a name}} {{9-10=}} expected-astgen-error {{initializers cannot have a name}}{{9-13=}}{{8-9=}} expected-astgen-error {{cannot find type 'T' in scope}}
}

struct InitializerWithLabels {
  init c d: Int {}
  // expected-cparser-error @-1 {{expected '(' for initializer parameters}} expected-astgen-error@-1 {{expected ')' to end parameter clause}}
  // expected-cparser-error @-2 {{initializer requires a body}} expected-astgen-error@-2 {{expected '(' to start parameter clause}}
}

// rdar://20337695
func f1() {

  // expected-cparser-error @+6 {{cannot find 'C' in scope}}
  // expected-cparser-note @+5 {{did you mean 'n'?}}
  // expected-cparser-error @+4 {{unary operator cannot be separated from its operand}} {{11-12=}}
  // expected-cparser-error @+3 {{'==' is not a prefix unary operator}}
  // expected-cparser-error @+2 {{consecutive statements on a line must be separated by ';'}} {{8-8=;}}
  // expected-error@+1 {{type annotation missing in pattern}}
  let n == C { get {}  // expected-cparser-error {{cannot find 'get' in scope}} expected-astgen-error {{unexpected code in function}}
  }
}


// <rdar://problem/20489838> QoI: Nonsensical error and fixit if "let" is missing between 'if let ... where' clauses
func testMultiPatternConditionRecovery(x: Int?) {
  // expected-cparser-error@+1 {{expected ',' joining parts of a multi-clause condition}} {{15-21=,}} expected-astgen-error@+1 {{expected ',' joining parts of a multi-clause condition}}{{15-16=}}{{22-22=, }}{{16-22=}}
  if let y = x where y == 0, let z = x {
    _ = y
    _ = z
  }

  if var y = x, y == 0, var z = x {
    z = y; y = z
  }

  if var y = x, z = x { // expected-cparser-error {{expected 'var' in conditional}} {{17-17=var }} expected-astgen-error {{expected 'var' in optional binding}}
    z = y; y = z
  }


  // <rdar://problem/20883210> QoI: Following a "let" condition with boolean condition spouts nonsensical errors
  guard let x: Int? = 1, x == 1 else {  }
  // expected-warning @-1 {{explicitly specified type 'Int?' adds an additional level of optional to the initializer, making the optional check always succeed}} {{16-20=Int}}
}

// rdar://20866942
func testRefutableLet() {
  var e : Int?
  let x? = e  // expected-cparser-error {{consecutive statements on a line must be separated by ';'}} {{8-8=;}}
  // expected-cparser-error @-1 {{expected expression}} expected-astgen-error@-1 {{unexpected code '? = e' in function}}
  // expected-error @-2 {{type annotation missing in pattern}}
}

// <rdar://problem/19833424> QoI: Bad error message when using Objective-C literals (@"Hello") in Swift files
let myString = @"foo" // expected-error {{string literals in Swift are not preceded by an '@' sign}} {{16-17=}}


// <rdar://problem/16990885> support curly quotes for string literals
// expected-cparser-error @+1 {{unicode curly quote found, replace with '"'}} {{35-38="}} expected-astgen-error @+1 {{unicode curly quote found; use '"' instead}}
let curlyQuotes1 = “hello world!” // expected-cparser-error {{unicode curly quote found, replace with '"'}} {{20-23="}} expected-astgen-error {{cannot find '“hello world!”' in scope}}

// expected-cparser-error @+1 {{unicode curly quote found, replace with '"'}} {{20-23="}} expected-astgen-error @+1 {{unicode curly quote found; use '"' instead}}
let curlyQuotes2 = “hello world!" // // expected-astgen-error {{cannot find '“hello world!"' in scope}}


// <rdar://problem/21196171> compiler should recover better from "unicode Specials" characters
let ￼tryx  = 123        // expected-cparser-error {{invalid character in source file}}  {{5-8= }} expected-astgen-error {{invalid character in source file}}


// <rdar://problem/21369926> Malformed Swift Enums crash playground service
enum Rank: Int {  // expected-error {{'Rank' declares raw type 'Int', but does not conform to RawRepresentable and conformance could not be synthesized}} expected-note {{add stubs for conformance}}
  case Ace = 1
  case Two = 2.1  // expected-error {{cannot convert value of type 'Double' to raw type 'Int'}}
}

// rdar://22240342 - Crash in diagRecursivePropertyAccess
class r22240342 {
  lazy var xx: Int = {
    foo {  // expected-error {{cannot find 'foo' in scope}}
      let issueView = 42
      issueView.delegate = 12
      
    }
    return 42
    }()
}


// <rdar://problem/22387625> QoI: Common errors: 'let x= 5' and 'let x =5' could use Fix-its
func r22387625() {
  let _= 5 // expected-error{{'=' must have consistent whitespace on both sides}} {{8-8= }}
  let _ =5 // expected-error{{'=' must have consistent whitespace on both sides}} {{10-10= }}
}
// https://github.com/apple/swift/issues/45723
do { //
  let _: Int= 5 // expected-error{{'=' must have consistent whitespace on both sides}} {{13-13= }}
  let _: Array<Int>= [] // expected-error{{'=' must have consistent whitespace on both sides}} {{20-20= }}
}


// <rdar://problem/23086402> Swift compiler crash in CSDiag
protocol A23086402 {
  var b: B23086402 { get }
}

protocol B23086402 {
  var c: [String] { get }
}

func test23086402(a: A23086402) {
  print(a.b.c + "") // should not crash but: expected-error {{}}
}

// <rdar://problem/23550816> QoI: Poor diagnostic in argument list of "print" (varargs related)
// The situation has changed. String now conforms to the RangeReplaceableCollection protocol
// and `ss + s` becomes ambiguous. Disambiguation is provided with the unavailable overload
// in order to produce a meaningful diagnostics. (Related: <rdar://problem/31763930>)
func test23550816(ss: [String], s: String) {
  print(ss + s)  // expected-error {{'+' is unavailable: Operator '+' cannot be used to append a String to a sequence of strings}}
}

// <rdar://problem/23719432> [practicalswift] Compiler crashes on &(Int:_)
func test23719432() {
  var x = 42
    &(Int:x) // expected-error {{'&' may only be used to pass an argument to inout parameter}}
}

// <rdar://problem/19911096> QoI: terrible recovery when using '·' for an operator
infix operator · {  // expected-error {{'·' is considered an identifier and must not appear within an operator name}} expected-astgen-error {{operator should not be declared with body}}
  associativity none precedence 150 // // expected-astgen-error {{consecutive statements on a line must be separated by newline or ';'}} expected-astgen-warning {{integer literal is unused}} expected-astgen-error {{cannot find 'precedence' in scope}}
} // expected-astgen-error {{unexpected brace in source file}}

infix operator -@-class Recover1 {} // expected-cparser-error {{'@' is not allowed in operator names}} expected-astgen-error {{'@-class' is not allowed in operator names}} expected-astgen-error {{cannot find 'Recover1' in scope; did you mean 'Decoder'?}}
prefix operator -фф--class Recover2 {} // expected-cparser-error {{'фф' is considered an identifier and must not appear within an operator name}} expected-astgen-error {{'фф--class' is not allowed in operator names}} expected-astgen-error {{cannot find 'Recover2' in scope}}

// <rdar://problem/21712891> Swift Compiler bug: String subscripts with range should require closing bracket.
func r21712891(s : String) -> String {
  let a = s.startIndex..<s.startIndex
  _ = a
  // The specific errors produced don't actually matter, but we need to reject this.
  return "\(s[a)"  // expected-cparser-error {{expected ']' in expression list}} expected-astgen-error {{expected ']' to end subscript}} expected-note {{to match this opening '['}}
}


// <rdar://problem/24029542> "Postfix '.' is reserved" error message" isn't helpful
func postfixDot(a : String) {
  _ = a.utf8
  _ = a.   utf8  // expected-cparser-error {{extraneous whitespace after '.' is not permitted}} {{9-12=}} expected-astgen-error {{extraneous whitespace after '.' is not permitted}}
  _ = a.       // expected-cparser-error {{expected member name following '.'}} expected-astgen-error {{expected name in member access}}
    a.         // expected-cparser-error {{expected member name following '.'}} expected-astgen-error {{expected name in member access}}
}

// <rdar://problem/22290244> QoI: "UIColor." gives two issues, should only give one
func f() { // expected-cparser-note 2{{did you mean 'f'?}} expected-astgen-note {{did you mean 'f'?}}
  _ = ClassWithStaticDecls.  // expected-cparser-error {{expected member name following '.'}} expected-astgen-error {{expected name in member access}}
}


// rdar://problem/22478168
// https://github.com/apple/swift/issues/53396
// expected-cparser-error@+1 {{expected '=' instead of '==' to assign default value for parameter}} {{21-23==}} expected-astgen-error@+1 {{expected '=' instead of '==' to assign default value for parameter}}{{21-24=}}{{24-24== }}
func f_53396(a: Int == 0) {}

// rdar://38225184
extension Collection where Element == Int && Index == Int {} //
// expected-cparser-error@-1 {{expected ',' to separate the requirements of this 'where' clause}} {{43-45=,}} expected-astgen-error@-1 {{expected ',' to separate the requirements of this 'where' clause}}{{43-46=}}{{42-43=}}{{46-46=, }}

func testSkipUnbalancedParen() {
  ?( // expected-cparser-error {{expected expression}} expected-astgen-error {{unexpected code '?(' in function}}
}
func testSkipToFindOpenBrace1() { // // expected-astgen-note {{to match this opening '{'}}
  // expected-error@+3 {{expected pattern}}
  // expected-cparser-error@+2 {{variable binding in a condition requires an initializer}}
  // expected-cparser-error@+1 {{expected '{' after 'if' condition}} expected-astgen-error@+1 {{expected '{' in 'if' statement}}
  do { if case }
}
func testSkipToFindOpenBrace2() { // // expected-astgen-note {{to match this opening '{'}}
  do { if true {} else false } // expected-error {{expected '{' or 'if' after 'else'}} expected-astgen-warning {{boolean literal is unused}}
}

struct Outer {
  struct Inner<T> {}
}
extension Outer.Inner<Never> { // expected-cparser-note {{in extension of 'Outer.Inner<Never>'}} expected-astgen-error {{declaration is only valid at file scope}}
  @someAttr // // expected-astgen-error {{expected declaration after attribute}}
} // expected-cparser-error {{expected declaration}} expected-astgen-error {{expected '}' to end function}} expected-astgen-error {{expected '}' to end function}}
