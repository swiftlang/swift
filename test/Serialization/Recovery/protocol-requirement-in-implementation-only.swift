// RUN: %empty-directory(%t)

//// Build the private module and the public module normally.
//// Force the public module to be system with an underlying Clang module.
// RUN: %target-swift-frontend -emit-module -DPRIVATE_LIB %s -module-name private_lib -emit-module-path %t/private_lib.swiftmodule
// RUN: %target-swift-frontend -emit-module -DPUBLIC_LIB %s -module-name public_lib -emit-module-path %t/public_lib.swiftmodule -I %t -I %S/Inputs/protocol-requirement-in-implementation-only -import-underlying-module

//// Printing the public module should not crash when reading the HiddenStruct typealias in `M`.
// RUN: %target-swift-ide-test -print-module -module-to-print=public_lib -source-filename=x -skip-overrides -I %t

#if PRIVATE_LIB

public struct HiddenStruct: Equatable {
  public init() {}
}

#elseif PUBLIC_LIB

@_implementationOnly import private_lib

protocol SomeProtocol {
  // Make sure we recover from failure when reading the type witness and
  // associated conformance.
  associatedtype Value: Equatable
  static var defaultValue: Value { get }
}
public struct M: SomeProtocol {
  typealias Value = HiddenStruct
  static let defaultValue = HiddenStruct()
}
#endif
