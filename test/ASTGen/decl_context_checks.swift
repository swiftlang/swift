// Declarations that used to crash ASTGen because a check the C++ parser performs
// was missing, letting a malformed declaration through to code that assumed it
// could not exist.

// RUN: %target-typecheck-verify-swift -parse-as-library \
// RUN:   -enable-experimental-feature ParserASTGen
// RUN: %target-typecheck-verify-swift -parse-as-library

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

// MARK: - 'static'/'class' outside a type

// Both parsers report this and then drop the static-ness. Keeping it produced a
// static property at file scope, which reaches getSelfInterfaceType() and
// asserts.
static let sl: Int = 0
// expected-error@-1 {{static properties may only be declared on a type}}
static var sv: Int = 0
// expected-error@-1 {{static properties may only be declared on a type}}
class var cv: Int = 0
// expected-error@-1 {{class properties may only be declared on a type}}

static func sf() {}
// expected-error@-1 {{static methods may only be declared on a type}}
class func cf() {}
// expected-error@-1 {{class methods may only be declared on a type}}

// A local context is not a type context either.
func localScope() {
  static let inner = 0
  // expected-error@-1 {{static properties may only be declared on a type}}
  _ = inner
}

// In a type context they are of course fine.
struct S {
  static let ok = 0
  static func okFunc() {}
}

extension S {
  static let alsoOk = 0
}

// MARK: - Non-module import without a module name

// 'import var x' names no module to import from. The C++ parser reports this and
// builds no ImportDecl; ImportDecl::create asserts the same invariant, which is
// what ASTGen hit.
import var x
// expected-error@-1 {{expected module name in import declaration}}
import struct y
// expected-error@-1 {{expected module name in import declaration}}

// MARK: - Keyword arguments where they are not allowed

// An operator, closure or enum case cannot have a parameter written with both an
// argument label and a parameter name. Both parsers report it and then keep only
// the parameter name; leaving the label in place gave a closure parameter an API
// name, which Sema asserts against.
let keywordArgClosure = {(y z: Int) in }
// expected-error@-1 {{closure cannot have keyword arguments}}

let keywordArgVariadicClosure = {(x: Int..., y z: Int) in }
// expected-error@-1 {{closure cannot have keyword arguments}}
// expected-error@-2 {{no parameters may follow a variadic parameter in a closure}}

enum HasKeywordArg {
  case foo(y z: Int)
  // expected-error@-1 {{enum case cannot have keyword arguments}}
}

func +++ (a b: Int, c d: Int) -> Int { 0 }
// expected-error@-1 {{operator cannot have keyword arguments}}
// expected-error@-2 {{operator cannot have keyword arguments}}
infix operator +++

// A regular function of course may have both.
func labelled(label name: Int) {}

// '_' is not a keyword argument: it means the parameter has no argument label,
// which is allowed in all three contexts.
enum UnderscoreLabel {
  case foo(_ s: Int)
}
let underscoreLabelClosure = {(_ x: Int) in }
func --- (_ a: Int, _ b: Int) -> Int { 0 }
infix operator ---
