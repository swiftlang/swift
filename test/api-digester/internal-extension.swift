// REQUIRES: VENDOR=apple



// RUN: %empty-directory(%t.mod)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t.module-cache)
// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -emit-module -o %t.mod/cake.swiftmodule %S/Inputs/cake.swift -parse-as-library -I %S/Inputs/ClangCake %clang-importer-sdk-nosource
// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -emit-module -o %t.mod/main.swiftmodule %s -parse-as-library -I %t.mod -I %S/Inputs/ClangCake %clang-importer-sdk-nosource
// RUN: %api-digester -dump-sdk -module main -o %t.dump.json -module-cache-path %t.module-cache -swift-only -I %t.mod -I %S/Inputs/ClangCake %clang-importer-sdk-nosource
// RUN: %FileCheck %s < %t.dump.json

import cake

// CHECK: publicSymbol
public func publicSymbol() {}

internal protocol InternalProto {}
public protocol PublicProto {}

// This internal extension doesn't declare any new members on S1, so it
// shouldn't show up at all in the output.
// CHECK-NOT: S1
internal extension S1 {
  var x: Int { return 4 }
}

// There should only be one conformance declared: the conformance of C0 to
// PublicProto. InternalProto should not show up in the list.
// CHECK:      "name": "C0",
// CHECK:      "conformances": [
// CHECK-NEXT: {
// CHECK-NEXT:   "kind": "Conformance",
// CHECK-NEXT:   "name": "Copyable",
// CHECK-NEXT:   "printedName": "Copyable",
// CHECK-NEXT:   "usr": "s:s8CopyableP",
// CHECK-NEXT:   "mangledName": "$ss8CopyableP"
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT:   "kind": "Conformance",
// CHECK-NEXT:   "name": "Escapable",
// CHECK-NEXT:   "printedName": "Escapable",
// CHECK-NEXT:   "usr": "s:s9EscapableP",
// CHECK-NEXT:   "mangledName": "$ss9EscapableP"
// CHECK-NEXT: },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "Conformance",
// CHECK-NEXT:     "name": "PublicProto",
// CHECK-NEXT:     "printedName": "PublicProto",
// CHECK-NEXT:     "usr": "s:4main11PublicProtoP",
// CHECK-NEXT:     "mangledName": "$s4main11PublicProtoP"
// CHECK-NEXT:   }
// CHECK-NEXT: ]
extension C0: InternalProto {
}
extension C0: PublicProto {
}
