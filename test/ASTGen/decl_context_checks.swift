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
