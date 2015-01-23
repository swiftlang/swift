// RUN: %target-parse-verify-swift

//===--- Helper types used in this file.

protocol FooProtocol {}

//===--- Tests.

func garbage() -> () {
  var a : Int
  ] this line is invalid, but we will stop at the keyword below... // expected-error{{expected expression}}
  return a + "a" // expected-error{{binary operator '+' cannot be applied to operands of type 'Int' and 'String'}} expected-note {{overloads for '+' exist with these partially matching parameter lists: (Int, Int), (String, String), (Int, UnsafeMutablePointer<T>), (Int, UnsafePointer<T>)}}
}

func moreGarbage() -> () {
  ) this line is invalid, but we will stop at the declaration... // expected-error{{expected expression}}
  func a() -> Int { return 4 }
  return a() + "a" // expected-error{{binary operator '+' cannot be applied to operands of type 'Int' and 'String'}} expected-note {{overloads for '+' exist with these partially matching parameter lists: (Int, Int), (String, String), (Int, UnsafeMutablePointer<T>), (Int, UnsafePointer<T>)}}
}


class Container<T> {
  func exists() -> Bool { return true }
}

func useContainer() -> () {
  var a : Container<not a type [skip this greater: >] >, b : Int // expected-error{{expected '>' to complete generic argument list}} expected-note{{to match this opening '<'}}
  b = 5 // no-warning
  a.exists() // expected-warnin
}


@xyz class BadAttributes { // expected-error{{unknown attribute 'xyz'}}
  func exists() -> Bool { return true }
}

func test(a: BadAttributes) -> () {
  a.exists() // no-warning
}

// Here is an extra random close-brace!
} // expected-error{{extraneous '}' at top level}} {{1-2=}}


//===--- Recovery for braced blocks.

func braceStmt1() {
  { braceStmt1(); } // expected-error {{braced block of statements is an unused closure}} expected-error {{expression resolves to an unused function}}
}

func braceStmt2() {
  { () in braceStmt2(); } // expected-error {{expression resolves to an unused function}}
}

func braceStmt3() {
  { // expected-error {{braced block of statements is an unused closure}}
    undefinedIdentifier {} // expected-error {{use of unresolved identifier 'undefinedIdentifier'}}
  }
}

//===--- Recovery for misplaced 'static'.

static func toplevelStaticFunc() {} // expected-error {{static methods may only be declared on a type}}

static struct StaticStruct {} // expected-error {{declaration cannot be marked 'static'}}
static class StaticClass {} // expected-error {{declaration cannot be marked 'static'}}
static protocol StaticProtocol {} // expected-error {{declaration cannot be marked 'static'}}
static typealias StaticTypealias = Int // expected-error {{declaration cannot be marked 'static'}}

class ClassWithStaticDecls {
  class var a = 42 // expected-error {{class stored properties not yet supported}}
}

//===--- Recovery for missing controlling expression in statements.

func missingControllingExprInIf() {
  if // expected-error {{expected expression, var, or let in 'if' condition}}

  if { // expected-error {{missing condition in an 'if' statement}}
  }

  if // expected-error {{missing condition in an 'if' statement}}
  {
  }

  if true {
  } else if { // expected-error {{missing condition in an 'if' statement}}
  }

  // It is debatable if we should do recovery here and parse { true } as the
  // body, but the error message should be sensible.
  if { true } { // expected-error {{missing condition in an 'if' statement}} expected-error {{braced block of statements is an unused closure}} expected-error{{type of expression is ambiguous without more context}}
  }

  // Ensure that we don't have recovery here.
  if { true }() {
  }
}

func missingControllingExprInWhile() {
  while // expected-error {{expected expression, var, or let in 'while' condition}}

  while { // expected-error {{missing condition in a 'while' statement}}
  }

  while // expected-error {{missing condition in a 'while' statement}}
  {
  }

  // It is debatable if we should do recovery here and parse { true } as the
  // body, but the error message should be sensible.
  while { true } { // expected-error {{missing condition in a 'while' statement}} expected-error {{braced block of statements is an unused closure}} expected-error{{type of expression is ambiguous without more context}}
  }

  // Ensure that we don't do recovery here.
  while { true }() {
  }
}

func missingControllingExprInDoWhile() {
  do {
  } while // expected-error {{missing condition in a 'while' statement}}
  { // expected-error {{braced block of statements is an unused closure}} expected-error {{expression resolves to an unused function}}
    missingControllingExprInDoWhile();
  }

  // Ensure that we don't do recovery here.
  do {
  } while { true }()
}

func acceptsClosure<T>(t: T) -> Bool { return true }

func missingControllingExprInFor() {
  for // expected-error {{expected initialization in a 'for' statement}}

  for { // expected-error {{missing initialization in a 'for' statement}}
  }

  for // expected-error {{missing initialization in a 'for' statement}}
  {
  }

  for var i { // expected-error 2{{expected ';' in 'for' statement}} expected-error {{type annotation missing in pattern}}
  }

  for ; { // expected-error {{expected ';' in 'for' statement}}
  }

  // FIXME: it would be better if this diagnostic appeared on the previous line.
  for ;
  { // expected-error {{expected ';' in 'for' statement}}
  }

  for ; true { // expected-error {{expected ';' in 'for' statement}}
  }

  for var i = 0; true { // expected-error {{expected ';' in 'for' statement}}
  }

  // Ensure that we don't do recovery in the following cases.
  for ; ; {
  }

  for { true }(); ; {
  }

  for ; { true }() ; {
  }

  for acceptsClosure { 42 }; ; {
  }

  // A trailing closure is not accepted for the condition.
  for ; acceptsClosure { 42 }; { // expected-error{{does not conform to protocol 'BooleanType'}} expected-error{{type of expression is ambiguous without more context}}
// expected-error@-1{{expected ';' in 'for' statement}}
// expected-error@-2{{braced block}}
  }
}

func missingControllingExprInForEach() {
  for in { // expected-error {{expected pattern}} expected-error {{expected SequenceType expression for for-each loop}}
  }

  for for in { // expected-error {{expected initialization in a 'for' statement}} expected-error {{expected pattern}} expected-error {{expected SequenceType expression for for-each loop}}
  }

  for i in { // expected-error {{expected SequenceType expression for for-each loop}}
  }
}

func missingControllingExprInSwitch() {
  switch // expected-error {{expected expression in 'switch' statement}} expected-error {{expected '{' after 'switch' subject expression}}

  switch { // expected-error {{expected expression in 'switch' statement}}
  } // expected-error {{'switch' statement body must have at least one 'case' or 'default' block}}

  switch // expected-error {{expected expression in 'switch' statement}}
  {
  } // expected-error {{'switch' statement body must have at least one 'case' or 'default' block}}

  // FIXME: we should not produce diagnostics about 'case' here, because we actually recover.
  switch { // expected-error {{expected expression in 'switch' statement}}
    case _: return // expected-error {{'case' label can only appear inside a 'switch' statement}}
  }

  switch { // expected-error {{expected expression in 'switch' statement}}
    case Int: return // expected-error {{'case' label can only appear inside a 'switch' statement}}
    case _: return // expected-error {{'case' label can only appear inside a 'switch' statement}}
  }

  // Ensure that we don't do recovery in the following cases.
  switch { 42 } {
    case _: return
  }

  switch { 42 }() {
    case _: return
  }
}

//===--- Recovery for missing braces in nominal type decls.

struct NoBracesStruct1() // expected-error {{expected '{' in struct}}
enum NoBracesUnion1() // expected-error {{expected '{' in enum}}
class NoBracesClass1() // expected-error {{expected '{' in class}}
protocol NoBracesProtocol1() // expected-error {{expected '{' in protocol type}}
extension NoBracesStruct1() // expected-error {{expected '{' in extension}}

struct NoBracesStruct2 // expected-error {{expected '{' in struct}}
enum NoBracesUnion2 // expected-error {{expected '{' in enum}}
class NoBracesClass2 // expected-error {{expected '{' in class}}
protocol NoBracesProtocol2 // expected-error {{expected '{' in protocol type}}
extension NoBracesStruct2 // expected-error {{expected '{' in extension}}

//===--- Recovery for parse errors in types.

struct ErrorTypeInVarDecl1 {
  var v1 : // expected-error {{expected type}}
}

struct ErrorTypeInVarDecl2 {
  var v1 : Int. // expected-error {{expected identifier in dotted type}} expected-error {{postfix '.' is reserved}}
  var v2 : Int
}

struct ErrorTypeInVarDecl3 {
  var v1 : Int< // expected-error {{expected type}}
  var v2 : Int
}

struct ErrorTypeInVarDecl4 {
  var v1 : Int<, // expected-error {{expected type}}
  var v2 : Int
}

struct ErrorTypeInVarDecl5 {
  var v1 : Int<Int // expected-error {{expected '>' to complete generic argument list}} expected-note {{to match this opening '<'}}
  var v2 : Int
}

struct ErrorTypeInVarDecl6 {
  var v1 : Int<Int, // expected-note {{to match this opening '<'}}
               Int // expected-error {{expected '>' to complete generic argument list}}
  var v2 : Int
}


struct ErrorTypeInVarDecl7 {
  var v1 : Int<Int, // expected-error {{expected type}}
  var v2 : Int
}

struct ErrorTypeInVarDecl8 {
  var v1 : protocol<FooProtocol // expected-error {{expected '>' to complete protocol composition type}} expected-note {{to match this opening '<'}}
  var v2 : Int
}

struct ErrorTypeInVarDecl9 {
  var v1 : protocol // expected-error {{expected '<' in protocol composition type}}
  var v2 : Int
}

struct ErrorTypeInVarDecl10 {
  var v1 : protocol<FooProtocol // expected-error {{expected '>' to complete protocol composition type}} expected-note {{to match this opening '<'}}
  var v2 : Int
}

struct ErrorTypeInVarDecl11 {
  var v1 : protocol<FooProtocol, // expected-error {{expected identifier for type name}}
  var v2 : Int
}

struct ErrorGenericParameterList1< // expected-error {{expected an identifier to name generic parameter}} expected-error {{expected '{' in struct}}

struct ErrorGenericParameterList2<T // expected-error {{expected '>' to complete generic parameter list}} expected-note {{to match this opening '<'}} expected-error {{expected '{' in struct}}

struct ErrorGenericParameterList3<T, // expected-error {{expected an identifier to name generic parameter}} expected-error {{expected '{' in struct}}

// Note: Don't move braces to a different line here.
struct ErrorGenericParameterList4< // expected-error {{expected an identifier to name generic parameter}}
{
}

// Note: Don't move braces to a different line here.
struct ErrorGenericParameterList5<T // expected-error {{expected '>' to complete generic parameter list}} expected-note {{to match this opening '<'}}
{
}

// Note: Don't move braces to a different line here.
struct ErrorGenericParameterList6<T, // expected-error {{expected an identifier to name generic parameter}}
{
}

struct ErrorTypeInVarDeclFunctionType1 {
  var v1 : () -> // expected-error {{expected type for function result}}
  var v2 : Int
}

struct ErrorTypeInVarDeclArrayType1 {
  var v1 : Int[+] // expected-error {{expected expression after unary operator}} expected-error {{expected expression for size of array type}}
  var v2 : Int
}

struct ErrorTypeInVarDeclArrayType2 {
  var v1 : Int[+ // expected-error {{expected ']' in array type}} expected-note {{to match this opening '['}} expected-error {{unary operator cannot be separated from its operand}} expected-error {{expected expression for size of array type}}
  var v2 : Int
}

struct ErrorTypeInVarDeclArrayType3 {
  var v1 : Int[ // expected-note {{to match this opening '['}}
      // expected-error @-1{{expected expression for size of array type}}
  var v2 : Int // expected-error {{expected ']' in array type}} expected-error {{'var' cannot appear nested inside another 'var' or 'let' pattern}} expected-error {{consecutive declarations on a line must be separated by ';'}} expected-error {{expected declaration}}
}

struct ErrorTypeInVarDeclArrayType4 {
  var v1 : Int[1 // expected-error {{expected ']' in array type}} expected-note {{to match this opening '['}}
  // expected-error @-1{{fixed-length arrays are not yet supported}}
}

struct ErrorInFunctionSignatureResultArrayType1 {
  func foo() -> Int[ { // expected-error {{expected '{' in body of function declaration}} expected-note {{to match this opening '['}}
    return [0]
  } // expected-error {{expected ']' in array type}}
}

struct ErrorInFunctionSignatureResultArrayType2 {
  func foo() -> Int[0 { // expected-error {{expected ']' in array type}} expected-note {{to match this opening '['}}
        // expected-error@-1{{fixed-length arrays are not yet supported}}
    return [0]
  }
}

struct ErrorInFunctionSignatureResultArrayType3 {
  func foo() -> Int[0] { // expected-error {{fixed-length arrays are not yet supported}}
    return [0]
  }
}

struct ErrorInFunctionSignatureResultArrayType4 {
  func foo() -> Int[0_1] { // expected-error {{fixed-length arrays are not yet supported}}
    return [0]
  }
}


struct ErrorInFunctionSignatureResultArrayType5 {
  func foo() -> Int[0b1] { // expected-error {{fixed-length arrays are not yet supported}}
    return [0]
  }
}

struct ErrorInFunctionSignatureResultArrayType6 {
  func foo() -> Int[0o1] { // expected-error {{fixed-length arrays are not yet supported}}
    return [0]
  }
}

struct ErrorInFunctionSignatureResultArrayType7 {
  func foo() -> Int[0x1] { // expected-error {{fixed-length arrays are not yet supported}}
    return [0]
  }
}

struct ErrorInFunctionSignatureResultArrayType8 {
  func foo() -> Int[1.0] { // expected-error {{expected expression for size of array type}}
    return [0]
  }
}

struct ErrorInFunctionSignatureResultArrayType9 {
  func foo() -> Int["1.0"] { // expected-error {{expected expression for size of array type}}
    return [0]
  }
}

struct ErrorInFunctionSignatureResultArrayType10 {
  func foo() -> Int[true] { // expected-error {{expected expression for size of array type}}
    return [0]
  }
}

struct ErrorInFunctionSignatureResultArrayType11 {
  func foo() -> Int[(a){a++}] { // expected-error {{consecutive declarations on a line must be separated by ';'}} expected-error {{expected ']' in array type}} expected-note {{to match this opening '['}} expected-error {{use of unresolved identifier 'a'}} expected-error {{expected declaration}}
              // expected-error @-1{{expected expression for size of array type}}
  }
}

struct ErrorInFunctionSignatureResultArrayType12 {
  var x = 0
  func foo() -> Int[x++] { // expected-error {{expected expression for size of array type}}
    return [0]
  }
}

struct ErrorInFunctionSignatureResultArrayType13 {
  var x = 0
  func foo() -> Int[self.x] { // expected-error {{expected expression for size of array type}}
    return [0]
  }
}

struct ErrorInFunctionSignatureResultArrayType14 {
  func foo() -> Int[true ? 1 : 0] { // expected-error {{expected expression for size of array type}}
    return [0]
  }
}

struct ErrorInFunctionSignatureResultArrayType15 {
  func foo() -> Int[(1, 2)] { // expected-error {{expected expression for size of array type}}
  }
}

// Note: If we decide to support integer constant expressions, this should pass
struct ErrorInFunctionSignatureResultArrayType16 {
  func foo() -> Int[1 && 1] { // expected-error {{expected expression for size of array type}}
    return [0]
  }
}

//===--- Recovery for missing initial value in var decls.

struct MissingInitializer1 {
  var v1 : Int = // expected-error {{expected initial value after '='}}
}

//===--- Recovery for expr-postfix.

func exprPostfix1(x : Int) {
  x. // expected-error {{postfix '.' is reserved}} expected-error {{expected member name following '.'}}
}

func exprPostfix2() {
  var x = .42 // expected-error {{expected identifier after '.' expression}}
}

//===--- Recovery for expr-super.

class Base {}

class ExprSuper1 {
  init() {
    super // expected-error {{expected '.' or '[' after 'super'}}
  }
}

class ExprSuper2 {
  init() {
    super. // expected-error {{postfix '.' is reserved}} expected-error {{expected identifier or 'init' after super '.' expression}}
  }
}

//===--- Recovery for braces inside a nominal decl.

struct BracesInsideNominalDecl1 {
  { // expected-error {{expected declaration}}
    aaa
  }
  typealias A = Int
}
func use_BracesInsideNominalDecl1() {
  // Ensure that the typealias decl is not skipped.
  var a : BracesInsideNominalDecl1.A // no-error
}

//===--- Recovery for wrong decl introducer keyword.

class WrongDeclIntroducerKeyword1 {
  notAKeyword() {} // expected-error {{expected declaration}}
  func foo() {}
  class func bar() {}
}

// <rdar://problem/18502220> [swift-crashes 078] parser crash on invalid cast in sequence expr
Base=1 as Base=1  // expected-error {{cannot assign to the result of this expression}}



// <rdar://problem/18634543> Parser hangs at swift::Parser::parseType
public enum TestA {
  public static func convertFromExtenndition(
    // expected-error@+3{{expected parameter type following ':'}}
    // expected-error@+2{{expected ',' separator}}
    // expected-error@+1{{use of undeclared type 's'}}
    s._core.count != 0, "Can't form a Character from an empty String")
}

public enum TestB {
  public static func convertFromExtenndition(
    // expected-error@+3{{expected parameter type following ':'}}
    // expected-error@+2{{expected ',' separator}}
    // expected-error@+1{{use of undeclared type 's'}}
    s._core.count ?= 0, "Can't form a Character from an empty String")
}



// <rdar://problem/18634543> Infinite loop and unbounded memory consumption in parser
class bar {}
var baz: bar
func foo1(bar != baz) {}
func foo2(bar! = baz) {}



// <rdar://problem/18662272> Infinite loop and unbounded memory consumption in parser
class Baz {}
class Bar<T> {}
func f1(a: Bar<Baz !>) {}
func f2(a: Bar<Baz /* some comment */ !>) {}


