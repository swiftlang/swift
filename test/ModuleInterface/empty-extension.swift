// Test that we don't print extensions to implementation-only imported types.

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module %s -DIOI_LIB -module-name IOILib -emit-module-path %t/IOILib.swiftmodule
// RUN: %target-swift-frontend -emit-module %s -DEXPORTED_LIB -module-name IndirectLib -emit-module-path %t/IndirectLib.swiftmodule -I %t
// RUN: %target-swift-frontend -emit-module %s -DLIB -module-name Lib -emit-module-path %t/Lib.swiftmodule -I %t

// RUN: %target-swift-frontend-typecheck -emit-module-interface-path %t/out.swiftinterface %s -I %t -swift-version 5 -enable-library-evolution
// RUN: %FileCheck %s < %t/out.swiftinterface

#if IOI_LIB

public struct IOIImportedType {
  public func foo() {}
}

#elseif EXPORTED_LIB

public struct ExportedType {
  public func foo() {}
}

#elseif LIB

@_exported import IndirectLib

public struct NormalImportedType {
  public func foo() {}
}

#else // Client

import Lib
@_implementationOnly import IOILib

public protocol PublicProto {
  func foo()
}
extension IOIImportedType : PublicProto {}
// CHECK-NOT: IOIImportedType

extension NormalImportedType : PublicProto {}
// CHECK: extension NormalImportedType

extension ExportedType : PublicProto {}
// CHECK: extension ExportedType

#endif
