// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %build-clang-importer-objc-overlays

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk-nosource) -I %t -I %S/Inputs/custom-modules -print-module -source-filename %s -module-to-print=NewtypeObjC > %t.printed.NewtypeObjC.txt
// RUN: %FileCheck %s -check-prefix=PRINT -strict-whitespace < %t.printed.NewtypeObjC.txt
// RUN: %target-typecheck-verify-swift -sdk %clang-importer-sdk -I %S/Inputs/custom-modules -I %t %s

// REQUIRES: objc_interop

// PRINT: class UsesGenericClassA : NSObject {
// PRINT:   func takeEnumValues(_ values: GenericClassA<NSString>)
// PRINT:   func takeEnumValuesArray(_ values: [ClosedEnum])
// PRINT: }

import NewtypeObjC

func useSpecializationOverNewtypes(a: UsesGenericClassA) {
  let gca = GenericClassA<NSString>()
  a.takeEnumValues(gca)
}
