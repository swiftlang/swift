// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency -target %target-swift-5.1-abi-triple
// REQUIRES: concurrency

@reasync protocol ReasyncProtocol {}

@reasync struct ReasyncStruct {}
// expected-error@-1 {{@reasync may only be used on 'protocol' declarations}}
