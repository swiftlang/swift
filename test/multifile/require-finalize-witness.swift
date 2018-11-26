// RUN: %target-swift-frontend -module-name test -emit-ir -primary-file %s %S/Inputs/require-finalize-witness-other.swift -sdk %sdk -o - | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

@objc class C: NSObject { }

protocol P {
  func foo(_: String)
}

// CHECK: define {{.*}} @"$S4test1CCAA1PA2aDP3fooyySSFTW"
extension C: P { }
