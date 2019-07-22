// RUN: %target-resilience-test
// REQUIRES: executable_test

// Check for the 'rth' tool itself, to make sure it's doing what we expect.

import rth

#if BEFORE
let clientIsAfter = false
#else
let clientIsAfter = true
#endif

let execPath = CommandLine.arguments.first!
#if os(Windows)
let execName = execPath.split(separator: "\\").last!
#else
let execName = execPath.split(separator: "/").last!
#endif
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
