// RUN: %target-typecheck-verify-swift %s

@_implementationOnly struct MyStruct {}
// expected-error@-1{{'@_implementationOnly' on a type requires '-enable-experimental-feature CheckImplementationOnly'}}
