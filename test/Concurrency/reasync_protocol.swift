// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency -disable-availability-checking
// REQUIRES: concurrency

@reasync protocol ReasyncProtocol {}

@reasync struct ReasyncStruct {}
// expected-error@-1 {{@reasync may only be used on 'protocol' declarations}}