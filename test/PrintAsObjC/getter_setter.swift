// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-ir -o %t/getter_setter.ir -emit-objc-header-path %t/getter_setter.h -import-objc-header %S/Inputs/propertyWithOddGetterSetterNames.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck --check-prefix="HEADER" %s < %t/getter_setter.h
// RUN: %FileCheck --check-prefix="IRCHECK1" %s < %t/getter_setter.ir
// RUN: %FileCheck --check-prefix="IRCHECK2" %s < %t/getter_setter.ir
//
// REQUIRES: objc_interop

import Foundation

// rdar://problem/55519276: Make sure we're using the right selector names as
// written in Objective-C.
public final class Saiyan: NSObject, PowerProtocol {
  public var level: Int64
  // HEADER: @property (nonatomic, getter=getPower, setter=setPower:) int64_t level;
  // IRCHECK1: METACLASS_DATA__TtC13getter_setter6Saiyan
  // IRCHECK1: selector_data(getPower)
  // IRCHECK1: selector_data(setPower:)
  // IRCHECK1: INSTANCE_METHODS__TtC13getter_setter6Saiyan
  // IRCHECK2: METACLASS_DATA__TtC13getter_setter6Saiyan
  // IRCHECK2-NOT: selector_data(level)
  // IRCHECK2-NOT: selector_data(setLevel:)
  // IRCHECK2: INSTANCE_METHODS__TtC13getter_setter6Saiyan
  public override init() {
    level = 9001
  }
}
