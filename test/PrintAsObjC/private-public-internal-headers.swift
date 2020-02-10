// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s -module-name foo -disable-objc-attr-requires-foundation-module -disable-emit-single-objc-header
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/foo.swiftmodule -typecheck -emit-objc-header-path %t/foo.h -disable-objc-attr-requires-foundation-module -disable-emit-single-objc-header
// RUN: %FileCheck %s --check-prefix=CHECK-INTERNAL < %t/foo.h
// RUN: %check-in-clang %t/foo.h

// RUN: %FileCheck %s --check-prefix=CHECK-PUBLIC < %t/foo-public.h
// RUN: %check-in-clang %t/foo-public.h

// RUN: %FileCheck %s --check-prefix=CHECK-PRIVATE < %t/foo-private.h
// RUN: %check-in-clang %t/foo-private.h

// REQUIRES: objc_interop

@objc public class PublicClass {
  @objc public func foo() {}
}

@objc class InternalClass {
  @objc func foo() {}
}

// CHECK-INTERNAL: #include "foo-public.h"
// CHECK-INTERNAL: #include "foo-private.h"
// CHECK-INTERNAL: SWIFT_CLASS("_TtC3foo13InternalClass")
// CHECK-INTERNAL: @interface InternalClass
// CHECK-INTERNAL: - (void)foo;
// CHECK-INTERNAL: @end

// CHECK-PUBLIC: SWIFT_CLASS("_TtC3foo11PublicClass")
// CHECK-PUBLIC: @interface PublicClass
// CHECK-PUBLIC: - (void)foo;
// CHECK-PUBLIC: @end

// CHECK-PRIVATE:  #include "foo-public.h"
