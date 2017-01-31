// RUN: rm -rf %t && mkdir -p %t

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/Inputs/depends-on-swift-framework-helper.swift -module-name OtherModule

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -import-objc-header %S/../Inputs/empty.h -emit-module -o %t %s -module-name main
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -import-objc-header %S/../Inputs/empty.h -parse-as-library %t/main.swiftmodule -typecheck -emit-objc-header-path %t/main.h

// RUN: %FileCheck %s < %t/main.h

// RUN: %check-in-clang -I %S/Inputs/custom-modules %t/main.h
// RUN: %check-in-clang -fno-modules -Qunused-arguments %t/main.h -include objc_generics.h

// REQUIRES: objc_interop

import Foundation
import objc_generics
import OtherModule

// CHECK-LABEL: @interface Test
public class Test: NSObject {
  // CHECK: - (void)testSimpleTypealias:(id <Fungible> _Nonnull)_;
  func testSimpleTypealias(_: AliasForFungible) {}
  // CHECK: - (void)testGenericTypealias:(FungibleContainer<id <Fungible>> * _Nonnull)_;
  func testGenericTypealias(_: FungibleContainer<AliasForFungible>) {}
} // CHECK: @end
