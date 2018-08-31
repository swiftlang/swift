// RUN: %target-typecheck-verify-swift
// REQUIRES: OS=macosx
// <rdar://problem/17669805> Suggest narrowing the range of bad availabile checks

import Foundation

@available(macOS 10.50.2, *)
func foo() { }

func useFoo() {
  if #available(macOS 10.50.1, *) {
    foo() // expected-error {{'foo()' is only available on OS X 10.50.2 or newer}} {{23-30=10.50.2}}
  }
}

func useFooDifferentSpelling() {
  if #available(OSX 10.50.1, *) {
    foo() // expected-error {{'foo()' is only available on OS X 10.50.2 or newer}} {{21-28=10.50.2}}
  }
}

func useFooAlreadyOkRange() {
  if #available(OSX 10.51, *) {
    foo()
  }
}

func useFooUnaffectedSimilarText() {
  if #available(iOS 10.50.10, OSX 10.50.1, *) {
    foo() // expected-error {{'foo()' is only available on OS X 10.50.2 or newer}} {{35-42=10.50.2}}
  }
}

func useFooWayOff() {
    // expected-note@-1{{add @available attribute to enclosing global function}}
  if #available(OSX 10.10, *) {
    foo() // expected-error {{'foo()' is only available on OS X 10.50.2 or newer}}
    // expected-note@-1{{add 'if #available' version check}}
  }
}

@available(OSX 10.50, *)
class FooUser {
  func useFoo() {
    foo() // expected-error {{'foo()' is only available on OS X 10.50.2 or newer}} {{16-21=10.50.2}}
  }
}

@available(OSX, introduced: 10.50, obsoleted: 10.50.4)
class FooUser2 {
  func useFoo() {
    foo() // expected-error {{'foo()' is only available on OS X 10.50.2 or newer}} {{29-34=10.50.2}}
  }
}

@available(OSX, introduced: 10.50, obsoleted: 10.50.4)
@objc
class FooUser3 : NSObject {
  func useFoo() {
    foo() // expected-error {{'foo()' is only available on OS X 10.50.2 or newer}} {{29-34=10.50.2}}
  }
}

@available(OSX, introduced: 10.50.4)
class FooUserOkRange {
  func useFoo() {
    foo()
  }
}
