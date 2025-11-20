// Please keep this file in alphabetical order!

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s -module-name generic -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/generic.swiftmodule -typecheck -verify -emit-objc-header-path %t/generic.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/generic.h
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t/generic.h
// RUN: %check-in-clang %t/generic.h

// REQUIRES: objc_interop

import ObjectiveC

// CHECK-LABEL: @interface ConcreteClass
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc @objcMembers class ConcreteClass {}

// CHECK-NOT: @interface GenericSubclass
// NEGATIVE-NOT: @interface GenericSubclass
class GenericSubclass<T> : ConcreteClass {}

// CHECK-NOT: @interface NonGenericSubclass
// NEGATIVE-NOT: @interface NonGenericSubclass
class NonGenericSubclass : GenericSubclass<ConcreteClass> {}

// CHECK-LABEL: @interface TopMoviesOfAllTime
// CHECK-NEXT: rambo
// CHECK-NEXT: init
// CHECK-NOT: rocky
// NEGATIVE-NOT: rocky
// CHECK-NEXT: @end
@objc @objcMembers class TopMoviesOfAllTime {
  func rambo(c: ConcreteClass) {}
  func rocky(c: NonGenericSubclass) {}
}
