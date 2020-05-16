// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -disable-availability-checking -enable-implicit-dynamic -enable-private-imports %S/Inputs/opaque_return_type_parameter.swift -module-name Repo -emit-module -emit-module-path %t/Repo.swiftmodule
// RUN: %target-swift-frontend -disable-availability-checking -I %t -module-name A -swift-version 5 -primary-file %s -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -disable-availability-checking -I %t -module-name A -swift-version 5 -primary-file %s -c -o %t/tmp.o
@_private(sourceFile: "opaque_return_type_parameter.swift") import Repo

// Make sure we are not emitting a replacement for the opaque result type used as parameter (Assoc).

// CHECK-NOT: @"\01l_unnamed_dynamic_replacements"{{.*}}(%swift.type_descriptor* ()*

extension Container {
  @_dynamicReplacement(for: update(arg:)) private func __update(arg: Assoc) {}
}
