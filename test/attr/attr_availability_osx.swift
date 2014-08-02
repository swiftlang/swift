// RUN: %swift -parse -target x86_64-apple-macosx10.10 %s -verify

@availability(OSX, introduced=10.5, deprecated=10.8, obsoleted=10.9,
              message="you don't want to do that anyway")
func doSomething() { }
// expected-note @-1{{'doSomething()' was obsoleted in OS X version 10.9.0}}

doSomething() // expected-error{{'doSomething()' is unavailable: you don't want to do that anyway}}
