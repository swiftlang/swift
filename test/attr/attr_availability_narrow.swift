// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macos10.15
// REQUIRES: OS=macosx
// <rdar://problem/17669805> Suggest narrowing the range of bad availabile checks

import Foundation

@available(macOS 50.2, *)
func foo() { }

func useFoo() {
  if #available(macOS 50.1, *) {
    foo() // expected-error {{'foo()' is only available in macOS 50.2 or newer}} {{-1:23-27=50.2}}
  }
}

func useFooDifferentSpelling() {
  if #available(OSX 50.1, *) {
    foo() // expected-error {{'foo()' is only available in macOS 50.2 or newer}} {{-1:21-25=50.2}}
  }
}

func useFooAlreadyOkRange() {
  if #available(OSX 51, *) {
    foo()
  }
}

func useFooUnaffectedSimilarText() {
  if #available(iOS 50.10, OSX 50.1, *) {
    foo() // expected-error {{'foo()' is only available in macOS 50.2 or newer}} {{-1:32-36=50.2}}
  }
}

func useFooWayOff() {
  // expected-note@-1 2 {{add '@available' attribute to enclosing global function}}
  if #available(OSX 10.10, *) {
    foo() // expected-error {{'foo()' is only available in macOS 50.2 or newer}}
    // expected-note@-1{{add 'if #available' version check}}
  }

  if #available(OSX 49.0, *) {
    foo() // expected-error {{'foo()' is only available in macOS 50.2 or newer}}
    // expected-note@-1{{add 'if #available' version check}}
  }
}

@available(OSX 50, *)
class FooUser {
  func useFoo() {
    foo() // expected-error {{'foo()' is only available in macOS 50.2 or newer}} {{-3:16-18=50.2}}
  }
}

@available(OSX, introduced: 50, obsoleted: 50.4)
class FooUser2 {
  func useFoo() {
    foo() // expected-error {{'foo()' is only available in macOS 50.2 or newer}} {{-3:29-31=50.2}}
  }
}

@available(OSX, introduced: 50, obsoleted: 50.4)
@objc
class FooUser3 : NSObject {
  func useFoo() {
    foo() // expected-error {{'foo()' is only available in macOS 50.2 or newer}} {{-4:29-31=50.2}}
  }
}

@available(OSX, introduced: 50.4)
class FooUserOkRange {
  func useFoo() {
    foo()
  }
}

@available(macOS 10.50.2, *)
func foo10() { }

func useFoo10() {
  if #available(macOS 10.50.1, *) {
    foo10() // expected-error {{'foo10()' is only available in macOS 10.50.2 or newer}} {{-1:23-30=10.50.2}}
  }
}

func useFoo10WayOff() {
    // expected-note@-1 3 {{add '@available' attribute to enclosing global function}}
  if #available(OSX 10.10, *) {
    foo10() // expected-error {{'foo10()' is only available in macOS 10.50.2 or newer}}
    // expected-note@-1{{add 'if #available' version check}}
  }

  if #available(OSX 10.49.0, *) {
    foo10() // expected-error {{'foo10()' is only available in macOS 10.50.2 or newer}}
    // expected-note@-1{{add 'if #available' version check}}
  }

  if #available(OSX 9, *) {
    foo10() // expected-error {{'foo10()' is only available in macOS 10.50.2 or newer}}
    // expected-note@-1{{add 'if #available' version check}}
  }
}
