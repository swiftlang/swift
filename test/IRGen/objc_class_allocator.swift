// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-ir %s -validate-tbd-against-ir=all | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

public class Foo {
  // CHECK-LABEL: define linkonce_odr hidden swiftcc ptr @"$sSo8NSNumberC5valueABSi_tcfC"
  // CHECK-LABEL: define linkonce_odr hidden swiftcc ptr @"$sSo8NSNumberC5valueABSi_tcfcTO"
  public var bar: NSNumber = NSNumber(value: 1)
}




