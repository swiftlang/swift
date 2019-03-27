// RUN: %target-resilience-test
// REQUIRES: executable_test

// Use swift-version 4.
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

// Check for the 'rth' tool itself, to make sure it's doing what we expect.

import rth

#if BEFORE
let clientIsAfter = false
#else
let clientIsAfter = true
#endif

let execPath = CommandLine.arguments.first!
// FIXME: Don't hardcode "/" here.
let execName = execPath.split(separator: "/").last!
switch execName {
case "after_after":
  precondition(clientIsAfter)
  precondition(libIsAfter)

case "before_after":
  precondition(clientIsAfter)
  precondition(!libIsAfter)

case "before_before":
  precondition(!clientIsAfter)
  precondition(!libIsAfter)

case "after_before":
  precondition(!clientIsAfter)
  precondition(libIsAfter)

default:
  fatalError("unknown exec name \(execName)")
}
