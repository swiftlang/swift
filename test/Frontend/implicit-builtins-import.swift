// This is just an example and documentation of how -parse-stdlib implicitly imports the Builtin module.
// Note that a lot of builtins expect some types to be defined in the stdlib, otherwise the builtin will be unavailable.
// Therefore "-module-name Swift" is significant (otherwise the types won't be find in the module we're building).
//
// Example:
//   %target-swift-emit-ir -parse-stdlib %s
//   Builtin.unreachable() // <<< error: module 'Builtin' has no member named 'unreachable'
//
// The real problem is that the 'unreachable' builtin needs the Never type (that's its return type).
//
// Example:
//   %target-swift-emit-ir -parse-stdlib -module-name Swift %s
//   enum Never {}
//   Builtin.unreachable() // works

// RUN: %target-swift-emit-ir -parse-stdlib -module-name Swift %s

enum Never {}

Builtin.unreachable()
Builtin.int_trap()
