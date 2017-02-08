// Please keep this file in alphabetical order!

// REQUIRES: objc_interop

// RUN: rm -rf %t && mkdir -p %t

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -import-objc-header %S/Inputs/circularity.h -emit-module -o %t %s
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -import-objc-header %S/Inputs/circularity.h -parse-as-library %t/circularity-errors.swiftmodule -typecheck -emit-objc-header-path %t/circularity-errors.h

// RUN: %FileCheck %s < %t/circularity-errors.h
// RUN: not %check-in-clang %t/circularity-errors.h

import Foundation

// CHECK: @protocol A2;
// CHECK-LABEL: @protocol A1 <Proto>
@objc protocol A1: Proto {
  // CHECK: - (void)test:
  func test(_: NeedsProto<A2>)
} // CHECK: @end
// CHECK-LABEL: @protocol A2 <Proto>
@objc protocol A2: Proto {
  // CHECK: - (void)test:
  func test(_: NeedsProto<A1>)
} // CHECK: @end

// CHECK: @class B2;
// CHECK-LABEL: @protocol B1 <Proto>
@objc protocol B1: Proto {
  // CHECK: - (void)test:
  @objc optional func test(_: NeedsProto<B2>)
} // CHECK: @end
// CHECK-LABEL: @interface B2 : ProtoImpl <B1>
class B2: ProtoImpl, B1 {
} // CHECK: @end

// CHECK: @class C1;
// Moved below.
class C1: ProtoImpl, C2 {}
// CHECK-LABEL: @protocol C2 <Proto>
@objc protocol C2: Proto {
  // CHECK: - (void)test:
  @objc optional func test(_: NeedsProto<C1>)
} // CHECK: @end
// CHECK-LABEL: @interface C1 : ProtoImpl <C2>
// CHECK: @end

// CHECK: @protocol D2;
// CHECK-LABEL: @protocol D1 <Proto>
@objc protocol D1: Proto {
  // CHECK: - (void)test:
  func test(_: NeedsProto<D2>)
} // CHECK: @end
// CHECK-LABEL: @protocol D2 <D1>
@objc protocol D2: D1 {
} // CHECK: @end

// CHECK: @protocol E1;
// Moved below.
@objc protocol E1: E2 {}
// CHECK-LABEL: @protocol E2 <Proto>
@objc protocol E2: Proto {
  // CHECK: - (void)test:
  func test(_: NeedsProto<E1>)
} // CHECK: @end
// CHECK-LABEL: @protocol E1 <E2>
// CHECK: @end
