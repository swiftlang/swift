// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-indexed-symbols -source-filename %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

// Make sure we properly handle missing optional requirements.
@objc protocol P {
  @objc optional func f()
  func g()
}

class C : P {
  @objc func g() {}
  // CHECK: [[@LINE-1]]:14 | instance-method(internal)/Swift | g() | s:14swift_ide_test1CC1gyyF | Def,Dyn,RelChild,RelOver | rel: 2
  // CHECK: RelOver | instance-method/Swift | g() | c:@M@swift_ide_test@objc(pl)P(im)g
  // CHECK: RelChild | class/Swift | C | s:14swift_ide_test1CC
}

// Make sure optional protocol methods in subclasses get RelOver to the
// protocol requirement. (https://github.com/swiftlang/swift/issues/56327)
class D : C {
  func f() {}
  // CHECK: [[@LINE-1]]:8 | instance-method(internal)/Swift | f() | s:14swift_ide_test1DC1fyyF | Def,Dyn,RelChild,RelOver | rel: 2
  // CHECK: RelOver | instance-method/Swift | f() | c:@M@swift_ide_test@objc(pl)P(im)f
  // CHECK: RelChild | class/Swift | D | s:14swift_ide_test1DC
}
