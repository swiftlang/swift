// RUN: %target-typecheck-verify-swift -solver-disable-enumerate-supertypes
// REQUIRES: objc_interop

import Foundation
import CoreFoundation

struct Exactly<T: ~Copyable> {}

func test<T: ~Copyable>(_: consuming T, _: consuming T) -> Exactly<T> { fatalError() }

func testTollFree1(x: CFString, y: NSString) -> Exactly<NSString> {
  let result = test(x, y)
  return result
}

func testTollFree2(x: NSString, y: CFString) -> Exactly<NSString> {
  let result = test(x, y)
  return result
}

func testTollFree3(x: CFNumber, y: NSNumber) -> Exactly<NSNumber> {
  let result = test(x, y)
  return result
}

func testTollFree4(x: NSNumber, y: CFNumber) -> Exactly<NSNumber> {
  let result = test(x, y)
  return result
}

func testTollFree3(x: CFBoolean, y: NSNumber) -> Exactly<NSNumber> {
  let result = test(x, y)
  return result
}

func testTollFree4(x: NSNumber, y: CFBoolean) -> Exactly<NSNumber> {
  let result = test(x, y)
  return result
}

func testTollFree5(x: CFNumber, y: CFBoolean) -> Exactly<CFNumber> {
  let result = test(x, y)
  // expected-error@-1 {{conflicting arguments to generic parameter 'T' ('CFNumber' vs. 'CFBoolean')}}
  return result
}

func testTollFree6(x: CFBoolean, y: CFNumber) -> Exactly<CFNumber> {
  let result = test(x, y)
  // expected-error@-1 {{conflicting arguments to generic parameter 'T' ('CFBoolean' vs. 'CFNumber')}}
  return result
}

func testTollFree7(x: CFBoolean, y: CFBoolean) -> Exactly<CFBoolean> {
  let result = test(x, y)
  return result
}
