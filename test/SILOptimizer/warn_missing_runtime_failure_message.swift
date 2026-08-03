// RUN: %target-swift-frontend -emit-ir -O -enable-builtin-module -disable-access-control -Wwarning MissingRuntimeFailureMessage -verify %s -o /dev/null
// The group is off by default, so nothing is reported without -Wwarning.
// RUN: %target-swift-frontend -emit-ir -O -enable-builtin-module -disable-access-control %s -o /dev/null 2>&1 | %FileCheck --allow-empty --check-prefix SILENT %s
// SILENT-NOT: warning

// A `Builtin.condfail_message` whose message operand cannot be resolved to a
// string literal loses that message: IRGenPrepare replaces it with the generic
// "unknown program error". Check that we warn about it, at the location of the
// failure rather than somewhere inside the standard library.

import Builtin
import Swift

// A literal message survives into the cond_fail, so there is nothing to warn
// about here.
public func literalMessage(_ cond: Builtin.Int1) {
  Builtin.condfail_message(cond, StaticString("literal message").unsafeRawPointer)
}

// An empty literal is still a literal.
public func emptyLiteralMessage(_ cond: Builtin.Int1) {
  Builtin.condfail_message(cond, StaticString("").unsafeRawPointer)
}

public func opaqueMessage(_ cond: Builtin.Int1, _ message: Builtin.RawPointer) {
  Builtin.condfail_message(cond, message) // expected-warning {{runtime failure message is not a string literal; the failure will be reported as 'unknown program error'}}
}

let global: StaticString = "from a global"

public func messageFromGlobal(_ cond: Builtin.Int1) {
  Builtin.condfail_message(cond, global.unsafeRawPointer) // expected-warning {{runtime failure message is not a string literal; the failure will be reported as 'unknown program error'}} expected-note {{message is produced here}}
}

// Both branches are literals, but the value reaching the builtin is a block
// argument, so no literal is available to fold.
public func messageFromBranch(_ cond: Builtin.Int1, _ which: Bool) {
  let message: StaticString = which ? "one" : "other"
  Builtin.condfail_message(cond, message.unsafeRawPointer) // expected-warning {{runtime failure message is not a string literal; the failure will be reported as 'unknown program error'}} expected-note {{message is produced here}}
}
