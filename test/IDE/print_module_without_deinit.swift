// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -skip-deinit=true -module-to-print=print_module_without_deinit -I %t -source-filename=%s | FileCheck -check-prefix=SKIP1 %s
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -skip-deinit=false -module-to-print=print_module_without_deinit -I %t -source-filename=%s | FileCheck -check-prefix=NOSKIP1 %s

import Foundation

// SKIP1: class PropertyOwnership {
// NOSKIP1: class PropertyOwnership {
class PropertyOwnership {
  // NOSKIP1-NEXT: deinit
  deinit {
  }
  // SKIP1-NEXT:    init()
  // NOSKIP1-NEXT:  init()

// SKIP1-NEXT:     }
// NOSKIP1-NEXT:   }
}

