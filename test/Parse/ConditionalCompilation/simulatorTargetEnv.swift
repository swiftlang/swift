// RUN: %swift -typecheck %s -verify -target x86_64-apple-ios7.0 -parse-stdlib
// RUN: %swift -typecheck %s -verify -target x86_64-unknown-linux-simulator -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target x86_64-apple-ios7.0

#if !targetEnvironment(simulator)
// This block should not parse.
let i: Int = "Hello"
#endif

#if targetEnvironment(simulator)
class C {}
var x = C()
#endif
var y = x
