// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t -parse-as-library %S/Inputs/use_imported_enums.swift -module-name UsesImportedEnums
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %t -emit-sil %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import UsesImportedEnums

// CHECK-LABEL: sil hidden @$s4main4test1eSbSo13NSRuncingModeV_tF
func test(e: NSRuncingMode) -> Bool {
  // CHECK-NOT: return
  // CHECK: $ss2eeoiySbx_xtSYRzSQ8RawValueRpzlF
  return compareImportedEnumToSelf(e)
}
