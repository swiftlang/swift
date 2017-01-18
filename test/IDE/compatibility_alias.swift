// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %build-clang-importer-objc-overlays

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk-nosource) -I %t -I %S/Inputs/custom-modules -print-module -source-filename %s -module-to-print=CompatibilityAlias > %t.printed.CompatibilityAlias.txt
// RUN: %FileCheck %s -check-prefix=PRINT -strict-whitespace < %t.printed.CompatibilityAlias.txt
// RUN: %target-typecheck-verify-swift -sdk %clang-importer-sdk -I %S/Inputs/custom-modules -I %t %s

// REQUIRES: objc_interop

// PRINT: typealias StringCheese = NSString

import CompatibilityAlias

func dontMove(cheese: StringCheese) -> StringCheese {
  return cheese
}

