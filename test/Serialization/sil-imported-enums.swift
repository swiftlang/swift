// RUN: rm -rf %t
// RUN: mkdir -p %t

// FIXME: BEGIN -enable-source-import hackaround
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %clang-importer-sdk-path/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %clang-importer-sdk-path/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -o %t -parse-as-library %S/Inputs/use_imported_enums.swift -module-name UsesImportedEnums
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-sil %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import UsesImportedEnums

// CHECK-LABEL: sil hidden @_T04main4testSbSC13NSRuncingModeO1e_tF
func test(e: NSRuncingMode) -> Bool {
  // CHECK-NOT: return
  // CHECK: _T0s2eeoiSbx_xts16RawRepresentableRzs9Equatable0B5ValueRpzlF
  return compareImportedEnumToSelf(e)
}
