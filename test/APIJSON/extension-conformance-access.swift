// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/ModuleCache)
// RUN: %target-swift-frontend %s -typecheck -parse-as-library -enable-library-evolution -module-name MyModule -package-name MyModule -swift-version 5 -library-level api -emit-api-descriptor-path %t/api.json
// RUN: %validate-json %t/api.json | %FileCheck %s

public protocol PublicProto {}
public protocol PublicProtoWithReq { func requirement() }

// A package type's conformances are package-level, whether declared inline or in
// an extension, so their symbols must be SPI.
package struct PackageStruct {}
extension PackageStruct: PublicProto {}
extension PackageStruct: PublicProtoWithReq {
  package func requirement() {}
}

package struct PackageInline: PublicProto {}

// Nested in a package type: effectively package.
package struct PackageOuter {
  public struct Inner {}
}
extension PackageOuter.Inner: PublicProto {}

// A public type's conformance to a public protocol is API.
public struct PublicStruct {}
extension PublicStruct: PublicProto {}

// An extension of an `@_spi` type is SPI as well.
@_spi(Private) public struct SPIStruct {}
extension SPIStruct: PublicProto {}

// CHECK:      "name": "_$s8MyModule12PackageOuterV5InnerVAA11PublicProtoAAMc",
// CHECK-NEXT: "access": "private",
// CHECK:      "name": "_$s8MyModule12PackageOuterV5InnerVAA11PublicProtoAAWP",
// CHECK-NEXT: "access": "private",
// CHECK:      "name": "_$s8MyModule12PublicStructVAA0C5ProtoAAMc",
// CHECK-NEXT: "access": "public",
// CHECK:      "name": "_$s8MyModule12PublicStructVAA0C5ProtoAAWP",
// CHECK-NEXT: "access": "public",
// CHECK:      "name": "_$s8MyModule13PackageInlineVAA11PublicProtoAAMc",
// CHECK-NEXT: "access": "private",
// CHECK:      "name": "_$s8MyModule13PackageInlineVAA11PublicProtoAAWP",
// CHECK-NEXT: "access": "private",
// CHECK:      "name": "_$s8MyModule13PackageStructVAA11PublicProtoAAMc",
// CHECK-NEXT: "access": "private",
// CHECK:      "name": "_$s8MyModule13PackageStructVAA11PublicProtoAAWP",
// CHECK-NEXT: "access": "private",
// CHECK:      "name": "_$s8MyModule13PackageStructVAA18PublicProtoWithReqAAMc",
// CHECK-NEXT: "access": "private",
// CHECK:      "name": "_$s8MyModule13PackageStructVAA18PublicProtoWithReqAAWP",
// CHECK-NEXT: "access": "private",
// CHECK:      "name": "_$s8MyModule9SPIStructVAA11PublicProtoAAMc",
// CHECK-NEXT: "access": "private",
// CHECK:      "name": "_$s8MyModule9SPIStructVAA11PublicProtoAAWP",
// CHECK-NEXT: "access": "private",
