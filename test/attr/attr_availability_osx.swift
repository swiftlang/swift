// RUN: %swift -parse -target x86_64-apple-macosx10.10 %s -verify

// XFAIL: linux

// REQUIRES: OS=macosx

@availability(OSX, introduced=10.5, deprecated=10.8, obsoleted=10.9,
              message="you don't want to do that anyway")
func doSomething() { }
// expected-note @-1{{'doSomething()' was obsoleted in OS X version 10.9}}

doSomething() // expected-error{{'doSomething()' is unavailable: you don't want to do that anyway}}


// Preservation of major.minor.micro
@availability(OSX, introduced=10.5, deprecated=10.8, obsoleted=10.9.1)
func doSomethingElse() { }
// expected-note @-1{{'doSomethingElse()' was obsoleted in OS X version 10.9.1}}

doSomethingElse() // expected-error{{'doSomethingElse()' is unavailable}}

// Preservation of minor-only version
@availability(OSX, introduced=8.0, deprecated=8.5, obsoleted=10)
func doSomethingReallyOld() { }
// expected-note @-1{{'doSomethingReallyOld()' was obsoleted in OS X version 10}}

doSomethingReallyOld() // expected-error{{'doSomethingReallyOld()' is unavailable}}
