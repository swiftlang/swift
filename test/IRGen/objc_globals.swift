// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/abi %s -whole-module-optimization -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/abi %s -emit-ir | %FileCheck %s
//
// REQUIRES: objc_interop
// REQUIRES: temporarily_disabled

import gadget
import Foundation

@inline(never)
func blackHole<T>(_ t: T) { }

// CHECK-LABEL: @"OBJC_CLASS_$_NSNumber" = external global %objc_class
// CHECK: @"OBJC_CLASS_$_NSString" = external global {{%.*}}, align
// CHECK: @"OBJC_CLASSLIST_REFERENCES_$_{{.*}}" = private global %struct._class_t* bitcast (%objc_class* @"OBJC_CLASS_$_NSNumber" to %struct._class_t*), section "__DATA,__objc_classrefs,regular,no_dead_strip"
// CHECK: @"OBJC_CLASSLIST_REFERENCES_$_{{.*}}" = private global %struct._class_t* bitcast (%objc_class* @"OBJC_CLASS_$_NSString" to %struct._class_t*), section "__DATA,__objc_classrefs,regular,no_dead_strip"

public func testLiterals() {
  blackHole(gadget.giveMeASelector())
  blackHole(gadget.giveMeANumber())
  blackHole(gadget.giveMeAMetaclass())
}

func fooInternal() {
  blackHole(NSString.self as AnyObject)
}

public func fooLazy() {
  fooInternal()
}

// CHECK-LABEL: define internal i8* @giveMeASelector()
// CHECK:         load i8*, i8** @OBJC_SELECTOR_REFERENCES_
// CHECK:         ret

// CHECK-LABEL: define internal {{.*}}* @giveMeANumber()
// CHECK:         [[CLASS:%.*]] = load %struct._class_t*, %struct._class_t**
// CHECK:         [[SELECTOR:%.*]] = load i8*, i8** @OBJC_SELECTOR_REFERENCES_.{{.*}}
// CHECK:         bitcast %struct._class_t* [[CLASS]] to i8*
// CHECK:         call {{.*}} @objc_msgSend
// CHECK:         ret

// CHECK-LABEL: define internal i8* @giveMeAMetaclass()
// CHECK:         [[CLASS:%.*]] = load %struct._class_t*, %struct._class_t**
// CHECK:         [[SELECTOR:%.*]] = load i8*, i8** @OBJC_SELECTOR_REFERENCES_
// CHECK:         bitcast %struct._class_t* [[CLASS]] to i8*
// CHECK:         call {{.*}} @objc_msgSend
// CHECK:         ret
