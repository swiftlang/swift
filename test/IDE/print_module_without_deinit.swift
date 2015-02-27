// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -skip-deinit=true -module-to-print=print_module_without_deinit -I %t -source-filename=%s | FileCheck -check-prefix=SKIP1 %s
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -skip-deinit=false -module-to-print=print_module_without_deinit -I %t -source-filename=%s | FileCheck -check-prefix=NOSKIP1 %s

import Foundation

// SKIP1: class PropertyOwnership {
// NOSKIP1: class PropertyOwnership {
class PropertyOwnership {
  // SKIP1-NEXT:  @NSCopying var copying: NSCopying?
  // NOSKIP1-NEXT:  @NSCopying var copying: NSCopying?
  @NSCopying var copying: NSCopying?

  // SKIP1-NEXT:  weak var weakVar: @sil_weak AnyObject?
  // NOSKIP1-NEXT:  weak var weakVar: @sil_weak AnyObject?
  weak var weakVar: AnyObject?

  // SKIP1-NEXT:  unowned var unownedVar: @sil_unowned PropertyOwnership
  // NOSKIP1-NEXT:  unowned var unownedVar: @sil_unowned PropertyOwnership
  unowned var unownedVar: PropertyOwnership

  // SKIP1-NEXT:  unowned(unsafe) var unownedUnsafeVar: @sil_unmanaged PropertyOwnership
  // NOSKIP1-NEXT:  unowned(unsafe) var unownedUnsafeVar: @sil_unmanaged PropertyOwnership
  unowned(unsafe) var unownedUnsafeVar: PropertyOwnership

  // SKIP1-NEXT:  init(other: PropertyOwnership)
  // NOSKIP1-NEXT:  init(other: PropertyOwnership)
  init(other: PropertyOwnership) {
    unownedVar = other
    unownedUnsafeVar = other
  }
  // NOSKIP1: deinit
  deinit {
  }
// SKIP1-NEXT:   }
// NOSKIP1-NEXT:   }
}

