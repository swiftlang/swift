// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/modules

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -module-name ImportAsMemberSwiftVersioned -o %t/modules/ImportAsMemberSwiftVersioned_a.partial.swiftmodule -swift-version 3 -I %S/../IDE/Inputs/custom-modules -primary-file %S/Inputs/ImportAsMemberSwiftVersioned_a.swift %S/Inputs/ImportAsMemberSwiftVersioned_b.swift

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -module-name ImportAsMemberSwiftVersioned -o %t/modules/ImportAsMemberSwiftVersioned_b.partial.swiftmodule -swift-version 3 -I %S/../IDE/Inputs/custom-modules -primary-file %S/Inputs/ImportAsMemberSwiftVersioned_b.swift %S/Inputs/ImportAsMemberSwiftVersioned_a.swift

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -merge-modules -swift-version 3 -emit-module -module-name ImportAsMemberSwiftVersioned -I %S/../IDE/Inputs/custom-modules -o %t/modules/ImportAsMemberSwiftVersioned.swiftmodule %t/modules/ImportAsMemberSwiftVersioned_a.partial.swiftmodule %t/modules/ImportAsMemberSwiftVersioned_b.partial.swiftmodule

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -swift-version 4 -O -I %S/../IDE/Inputs/custom-modules -o - %s -I %t/modules | %FileCheck %s

// REQUIRES: objc_interop
import Foundation
import ImportAsMember.Class
import ImportAsMemberSwiftVersioned

// CHECK: function_ref {{.*}}call_foo
public func callFoo() -> Any {
  return call_foo()
}
