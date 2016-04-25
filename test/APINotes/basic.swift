// RUN: %target-parse-verify-swift -I %S/Inputs/custom-modules
import APINotesTest

func testSwiftName() {
  moveTo(x: 0, y: 0, z: 0)
  moveTo(0, 0, 0) // expected-error{{missing argument labels 'x:y:z:' in call}}

  _ = global
  _ = ANTGlobalValue // expected-error{{use of unresolved identifier 'ANTGlobalValue'}}

  let ps = Point(x: 0.0, y: 0.0)
  let ps2 = PointStruct(x: 0.0, y: 0.0) // expected-error{{use of unresolved identifier 'PointStruct'}}
  let r: Real = 0.0
  let r2: real_t = 0.0 // expected-error{{use of undeclared type 'real_t'}}

  let rect: Rect
  let rect2: RectStruct // expected-error{{use of undeclared type 'RectStruct'}}

  let d: Double = __will_be_private
}
