// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %clang-importer-sdk -typecheck %t/main.swift -I %t/include -verify -swift-version 5
// RUN: %target-swift-frontend %clang-importer-sdk -typecheck %t/main.swift -I %t/include -verify -swift-version 5 -enable-upcoming-feature MemberImportVisibility

// RUN: %target-swift-ide-test %clang-importer-sdk -print-module -module-to-print Conformances -I %t/include -source-filename %t/main.swift | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_MemberImportVisibility

//--- include/module.modulemap
module Interface {
  header "Interface.h"
  export *
}
module VisibleCategory {
  header "VisibleCategory.h"
  export *
}
module Conformances {
  header "Conformances.h"
  export *
}
module OtherCategory {
  header "OtherCategory.h"
  export *
}

//--- include/Interface.h
@import Foundation;

@interface X : NSObject
- (void)methodFromInterface __attribute__((deprecated("Interface.h")));

@property (readonly, nonatomic) int readonlyPropFromInterface __attribute__((deprecated("Interface.h")));
@property (readwrite, nonatomic) int readwritePropFromInterface __attribute__((deprecated("Interface.h")));
- (int)methodAsPropertyWitnessFromInterface __attribute__((deprecated("Interface.h")));
@end

@interface X (Interface)
- (void)methodFromInterfaceCategory __attribute__((deprecated("Interface.h category")));
@property (readonly, nonatomic) int propFromInterfaceCategory __attribute__((deprecated("Interface.h category")));
@end

//--- include/VisibleCategory.h
@import Interface;

@interface X (VisibleCategory)
- (void)methodFromVisibleCategory __attribute__((deprecated("VisibleCategory.h")));

@property (readonly, nonatomic) int readonlyPropFromVisibleCategory __attribute__((deprecated("VisibleCategory.h")));
@property (readonly, nonatomic) int propAsMethodWitnessFromVisibleCategory __attribute__((deprecated("VisibleCategory.h")));
@end

//--- include/Conformances.h
@import VisibleCategory;

@protocol Proto

// Requirements witnessed by X's @interface.
- (void)methodFromInterface;
@property (readonly, nonatomic) int readonlyPropFromInterface;
@property (readwrite, nonatomic) int readwritePropFromInterface;
@property (readonly, nonatomic) int methodAsPropertyWitnessFromInterface;

// Requirements witnessed by a category in the same module as X's @interface.
- (void)methodFromInterfaceCategory;
@property (readonly, nonatomic) int propFromInterfaceCategory;

// Requirements witnessed by a category in the module 'VisibleCategory', which
// is imported by the module declaring the conformance.
- (void)methodFromVisibleCategory;
@property (readonly, nonatomic) int readonlyPropFromVisibleCategory;
- (int)propAsMethodWitnessFromVisibleCategory;

// Requirements witnessed by a category in the module 'OtherCategory', which
// is no visible from the module declaring the conformance.
- (void)methodFromOtherCategory;
@property (readonly, nonatomic) int propFromOtherCategory;

@end

@interface X (Proto) <Proto>
@end

//--- include/OtherCategory.h
@import Interface;

@interface X (OtherCategory)
- (void)methodFromOtherCategory __attribute__((deprecated("OtherCategory.h")));
@property (readonly, nonatomic) int propFromOtherCategory __attribute__((deprecated("OtherCategory.h")));
@end

//--- main.swift
import Conformances
import OtherCategory

func test(x: X) {
  x.methodFromInterface()
  // expected-warning@-1 {{'methodFromInterface()' is deprecated: Interface.h}}
  x.methodFromVisibleCategory()
  // expected-warning@-1 {{'methodFromVisibleCategory()' is deprecated: VisibleCategory.h}}
  _ = x.readonlyPropFromInterface
  // expected-warning@-1 {{'readonlyPropFromInterface' is deprecated: Interface.h}}
  _ = x.readonlyPropFromVisibleCategory
  // expected-warning@-1 {{'readonlyPropFromVisibleCategory' is deprecated: VisibleCategory.h}}
  _ = x.readwritePropFromInterface
  // expected-warning@-1 {{'readwritePropFromInterface' is deprecated: Interface.h}}
  x.readwritePropFromInterface = 0
  // expected-warning@-1 {{'readwritePropFromInterface' is deprecated: Interface.h}}
  _ = x.methodAsPropertyWitnessFromInterface()
  // expected-warning@-1 {{'methodAsPropertyWitnessFromInterface()' is deprecated: Interface.h}}
  _ = x.propAsMethodWitnessFromVisibleCategory()
  x.methodFromInterfaceCategory()
  // expected-warning@-1 {{'methodFromInterfaceCategory()' is deprecated: Interface.h category}}
  _ = x.propFromInterfaceCategory
  // expected-warning@-1 {{'propFromInterfaceCategory' is deprecated: Interface.h category}}
  x.methodFromOtherCategory()
  _ = x.propFromOtherCategory
}

// CHECK:      @_exported import VisibleCategory
// CHECK-EMPTY:
// CHECK-NEXT: protocol Proto {
// CHECK-NEXT:   func methodFromInterface()
// CHECK-NEXT:   var readonlyPropFromInterface: Int32 { get }
// CHECK-NEXT:   var readwritePropFromInterface: Int32 { get set }
// CHECK-NEXT:   var methodAsPropertyWitnessFromInterface: Int32 { get }
// CHECK-NEXT:   func methodFromInterfaceCategory()
// CHECK-NEXT:   var propFromInterfaceCategory: Int32 { get }
// CHECK-NEXT:   func methodFromVisibleCategory()
// CHECK-NEXT:   var readonlyPropFromVisibleCategory: Int32 { get }
// CHECK-NEXT:   func propAsMethodWitnessFromVisibleCategory() -> Int32
// CHECK-NEXT:   func methodFromOtherCategory()
// CHECK-NEXT:   var propFromOtherCategory: Int32 { get }
// CHECK-NEXT: }
// CHECK-NEXT: extension X : Proto {
// CHECK-NEXT:   var propFromOtherCategory: Int32 { get }
// CHECK-NEXT:   func propAsMethodWitnessFromVisibleCategory() -> Int32
// CHECK-NEXT:   func methodFromOtherCategory()
// CHECK-NEXT: }
