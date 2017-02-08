// RUN: %target-typecheck-verify-swift -I %S/Inputs/custom-modules -F %S/Inputs/custom-frameworks -verify-ignore-unknown
import APINotesTest
import APINotesFrameworkTest

func testSwiftName() {
  moveTo(x: 0, y: 0, z: 0)
  moveTo(0, 0, 0) // expected-error{{missing argument labels 'x:y:z:' in call}}

  _ = global
  _ = ANTGlobalValue // expected-error{{'ANTGlobalValue' has been renamed to 'global'}}

  let ps = Point(x: 0.0, y: 0.0)
  let ps2 = PointStruct(x: 0.0, y: 0.0) // expected-error{{'PointStruct' has been renamed to 'Point'}}
  let r: Real = 0.0
  let r2: real_t = 0.0 // expected-error{{'real_t' has been renamed to 'Real'}}

  let rect: Rect
  let rect2: RectStruct // expected-error{{'RectStruct' has been renamed to 'Rect'}}

  let d: Double = __will_be_private

  // From APINotesFrameworkTest.
  jumpTo(x: 0, y: 0, z: 0)
  jumpTo(0, 0, 0) // expected-error{{missing argument labels 'x:y:z:' in call}}
}

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected note produced: 'ANTGlobalValue' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: 'PointStruct' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: 'real_t' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: 'RectStruct' was obsoleted in Swift 3
