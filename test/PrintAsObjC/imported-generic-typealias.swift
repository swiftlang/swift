// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -parse-as-library %s -typecheck -emit-objc-header-path %t/imported-generic-typealias.h -import-objc-header %S/Inputs/imported-generic-typealias.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=CHECK-PUBLIC %s < %t/imported-generic-typealias.h

@objc public class MyRedBarn : Barn {
  @objc public func feed(_: Horse<NSObject>.Hay) {}
}

// CHECK-LABEL: SWIFT_CLASS("_TtC4main9MyRedBarn")
// CHECK-NEXT: @interface MyRedBarn : Barn
// CHECK-NEXT: - (void)feed:(Hay)_;
// CHECK-NEXT: @end
