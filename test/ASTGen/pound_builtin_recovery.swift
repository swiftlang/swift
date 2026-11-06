// Malformed built-in pound expressions that used to crash ASTGen.
//
// Both parsers must reject each of these, with the same primary diagnostic.
// Beyond that first diagnostic they resynchronize differently and emit
// different cascades, and recovery shape is not something ASTGen has to
// reproduce -- so this checks the diagnostics that matter rather than pinning
// every line with `-verify`.

// RUN: not %target-swift-frontend -typecheck -parse-as-library %s \
// RUN:   -enable-experimental-feature ParserASTGen 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -typecheck -parse-as-library %s 2>&1 | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

import Foundation

@objc class K: NSObject {
  @objc var p = 0
  @objc func m() {}
}

// MARK: - #selector

// CHECK-DAG: [[@LINE+1]]:31: error: expected expression naming a method within '#selector(...)'
let noSelectorArg = #selector()

// Only 'getter:' and 'setter:' are labels here; anything else is parsed as the
// method expression, so the ':' is where a ')' was expected.
// CHECK-DAG: [[@LINE+1]]:39: error: expected ')' to complete '#selector' expression
let badSelectorLabel = #selector(bogus: K.m)

// CHECK-DAG: [[@LINE+1]]:45: error: expected ')' to complete '#selector' expression
let extraSelectorArg = #selector(getter: K.p, K.m)

// A generic argument clause has no grammar after the keyword, so the '(' is
// what is reported missing.
// CHECK-DAG: [[@LINE+1]]:32: error: expected '(' following '#selector'
let genericSelector = #selector<Int>(K.m)

// MARK: - #keyPath

// CHECK-DAG: [[@LINE+1]]:29: error: expected property or type name within '#keyPath(...)'
let noKeyPathArg = #keyPath()

// Not a dotted chain of names.
// CHECK-DAG: [[@LINE+1]]:32: error: expected property or type name within '#keyPath(...)'
let keyPathOperator = #keyPath(1 + 2)

// CHECK-DAG: [[@LINE+1]]:30: error: expected property or type name within '#keyPath(...)'
let keyPathParens = #keyPath((K.p))

// An implicit member reference has no base to walk into.
// CHECK-DAG: [[@LINE+1]]:38: error: expected property or type name within '#keyPath(...)'
let keyPathImplicitMember = #keyPath(.p)

// CHECK-DAG: [[@LINE+1]]:34: error: expected ')' to complete '#keyPath' expression
let keyPathLabel = #keyPath(label: K.p)

// CHECK-DAG: [[@LINE+1]]:35: error: expected ')' to complete '#keyPath' expression
let extraKeyPathArg = #keyPath(K.p, K.p)

// CHECK-DAG: [[@LINE+1]]:30: error: expected '(' following '#keyPath'
let genericKeyPath = #keyPath<Int>(K.p)

// MARK: - #error / #warning carrying attributes or modifiers

// A built-in pound is not a declaration, so it takes neither. Both parsers
// report that a declaration was expected and still evaluate the directive.
class WithModifier {
  private
  // CHECK-DAG: [[@LINE+2]]:3: error: expected declaration
  // CHECK-DAG: [[@LINE+1]]:12: warning: modifier on a pound directive
  #warning("modifier on a pound directive")
  func member() {}
}
