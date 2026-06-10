// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -I %S/Inputs/custom-modules -F %S/Inputs/custom-frameworks
import APINotesTest
import APINotesFrameworkTest

#if _runtime(_ObjC)
extension A {
  func implicitlyObjC() { }
}

extension C {
  func alsoImplicitlyObjC() { }
}

class D : C {
  func yetAnotherImplicitlyObjC() { }
}

func testSelectors(a: AnyObject) {
  a.implicitlyObjC?()  // okay: would complain without SwiftObjCMembers
  a.alsoImplicitlyObjC?()  // okay: would complain without SwiftObjCMembers
  a.yetAnotherImplicitlyObjC?()  // okay: would complain without SwiftObjCMembers
}
#endif

func testSwiftName() {
  moveTo(x: 0, y: 0, z: 0)
  moveTo(0, 0, 0) // expected-error{{missing argument labels 'x:y:z:' in call}}

  _ = global
  _ = ANTGlobalValue // expected-error{{'ANTGlobalValue' has been renamed to 'global'}}
  _ = `test raw`
  _ = ANTGlobalValue2 // expected-error{{'ANTGlobalValue2' has been renamed to '`test raw`'}}
  _ = `Complex.Name`
  _ = ANTGlobalValue3 // expected-error{{'ANTGlobalValue3' has been renamed to '`Complex.Name`'}}
  _ = `class`
  _ = ANTGlobalValue4 // expected-error{{'ANTGlobalValue4' has been renamed to '`class`'}}
  `jump to`(x: 0.0)
  jumpToPoint(0.0) // expected-error{{'jumpToPoint' has been renamed to '`jump to`(x:)'}}
  _ = `3test raw`
  _ = ANTGlobalValue5 // expected-error{{'ANTGlobalValue5' has been renamed to '`3test raw`'}}

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
