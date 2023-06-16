// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %use_no_opaque_pointers -emit-irgen %s -I %t -I %S/Inputs/has_symbol -module-name test | %FileCheck %s
// RUN: %target-swift-frontend -emit-irgen %s -I %t -I %S/Inputs/has_symbol -module-name test

// REQUIRES: objc_interop

@_weakLinked import has_symbol_helper_objc

public func testClassTypes() {
  // CHECK: %{{[0-9]+}} = call i1 @"$sSo9ObjCClassCTwS"()
  if #_hasSymbol(ObjCClass.self) {}
}

// CHECK: define linkonce_odr hidden i1 @"$sSo9ObjCClassCTwS"()
// CHECK:   ret i1 icmp ne (%objc_class* @"OBJC_CLASS_$_ObjCClass", %objc_class* null)
