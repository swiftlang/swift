// RUN: %target-typecheck-verify-swift -I %S/Inputs/custom-modules -F %S/Inputs/custom-frameworks
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
