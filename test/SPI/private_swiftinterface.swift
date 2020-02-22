/// Test the generated private textual module interfaces and that the public
/// one doesn't leak SPI decls and info.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/spi_helper.swift -module-name SPIHelper -emit-module-path %t/SPIHelper.swiftmodule -swift-version 5 -enable-library-evolution -emit-module-interface-path %t/SPIHelper.swiftinterface -emit-private-module-interface-path %t/SPIHelper.private.swiftinterface

/// Make sure that the public swiftinterface of spi_helper doesn't leak SPI.
// RUN: %FileCheck -check-prefix=CHECK-HELPER %s < %t/SPIHelper.swiftinterface
// CHECK-HELPER-NOT: HelperSPI
// CHECK-HELPER-NOT: @_spi

/// Test the textual interfaces generated from this test.
// RUN: %target-swift-frontend -typecheck %s -emit-module-interface-path %t/main.swiftinterface -emit-private-module-interface-path %t/main.private.swiftinterface -enable-library-evolution -swift-version 5 -I %t
// RUN: %FileCheck -check-prefix=CHECK-PUBLIC %s < %t/main.swiftinterface
// RUN: %FileCheck -check-prefix=CHECK-PRIVATE %s < %t/main.private.swiftinterface

/// Serialize and deserialize this module, then print.
// RUN: %target-swift-frontend -emit-module %s -emit-module-path %t/merged-partial.swiftmodule -swift-version 5 -I %t -module-name merged -enable-library-evolution
// RUN: %target-swift-frontend -merge-modules %t/merged-partial.swiftmodule -module-name merged -sil-merge-partial-modules -emit-module -emit-module-path %t/merged.swiftmodule -I %t -emit-module-interface-path %t/merged.swiftinterface -emit-private-module-interface-path %t/merged.private.swiftinterface -enable-library-evolution -swift-version 5 -I %t
// RUN: %FileCheck -check-prefix=CHECK-PUBLIC %s < %t/merged.swiftinterface
// RUN: %FileCheck -check-prefix=CHECK-PRIVATE %s < %t/merged.private.swiftinterface

@_spi(HelperSPI) @_spi(OtherSPI) import SPIHelper
// CHECK-PUBLIC: import SPIHelper
// CHECK-PRIVATE: @_spi(OtherSPI) @_spi(HelperSPI) import SPIHelper

public func foo() {}
// CHECK-PUBLIC: foo()
// CHECK-PRIVATE: foo()

@_spi(MySPI) @_spi(MyOtherSPI) public func localSPIFunc() {}
// CHECK-PRIVATE: @_spi(MySPI)
// CHECK-PRIVATE: localSPIFunc()
// CHECK-PUBLIC-NOT: localSPIFunc()

// SPI declarations
@_spi(MySPI) public class SPIClassLocal {}
// CHECK-PRIVATE: @_spi(MySPI) public class SPIClassLocal
// CHECK-PUBLIC-NOT: class SPIClassLocal

@_spi(MySPI) public extension SPIClassLocal {
// CHECK-PRIVATE: @_spi(MySPI) extension SPIClassLocal
// CHECK-PUBLIC-NOT: extension SPIClassLocal

  @_spi(MySPI) func extensionMethod() {}
  // CHECK-PRIVATE: @_spi(MySPI) public func extensionMethod
  // CHECK-PUBLIC-NOT: func extensionMethod

  func internalExtensionMethod() {}
  // CHECK-PRIVATE-NOT: internalExtensionMethod
  // CHECK-PUBLIC-NOT: internalExtensionMethod
}

class InternalClassLocal {}
// CHECK-PRIVATE-NOT: InternalClassLocal
// CHECK-PUBLIC-NOT: InternalClassLocal

private class PrivateClassLocal {}
// CHECK-PRIVATE-NOT: PrivateClassLocal
// CHECK-PUBLIC-NOT: PrivateClassLocal

@_spi(LocalSPI) public func useOfSPITypeOk(_ p: SPIClassLocal) -> SPIClassLocal {
  fatalError()
}
// CHECK-PRIVATE: @_spi(LocalSPI) public func useOfSPITypeOk
// CHECK-PUBLIC-NOT: useOfSPITypeOk

@_spi(LocalSPI) extension SPIClass {
  // CHECK-PRIVATE: @_spi(LocalSPI) extension SPIClass
  // CHECK-PUBLIC-NOT: SPIClass

  @_spi(LocalSPI) public func extensionSPIMethod() {}
  // CHECK-PRIVATE: @_spi(LocalSPI) public func extensionSPIMethod()
  // CHECK-PUBLIC-NOT: extensionSPIMethod
}
