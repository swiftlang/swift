// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %clang-importer-sdk-path/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %clang-importer-sdk-path/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -o %t -parse-as-library %S/Inputs/use_imported_enums.swift -module-name UsesImportedEnums
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-sil %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import UsesImportedEnums

// CHECK-LABEL: sil hidden @$S4main4test1eSbSo13NSRuncingModeV_tF
func test(e: NSRuncingMode) -> Bool {
  // CHECK-NOT: return
  // CHECK: $Ss2eeoiySbx_xtSYRzSQ8RawValueRpzlF
  return compareImportedEnumToSelf(e)
}
