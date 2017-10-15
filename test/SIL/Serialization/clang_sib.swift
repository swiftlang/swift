// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sib -o %t -primary-file %s -module-name main
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -o /dev/null -primary-file %t/clang_sib.sib -module-name main

// REQUIRES: objc_interop

import Foundation

func test() {
  // Use a factory initializer.
  _ = Hive(queen: Bee())
}
