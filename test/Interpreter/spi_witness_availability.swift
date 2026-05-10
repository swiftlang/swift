// RUN: %target-run-simple-swift(-target %target-cpu-macos10.15) | \
// RUN:   %FileCheck %s --check-prefix=CHECK-CONFORMANCES-NOT-AVAILABLE
// RUN: %target-run-simple-swift(-target %target-cpu-macos10.15.4) | \
// RUN:   %FileCheck %s --check-prefix=CHECK-CONFORMANCES-AVAILABLE
// RUN: %target-run-simple-swift(-target %target-cpu-macos10.15.4 -library-level api) | \
// RUN:   %FileCheck %s --check-prefix=CHECK-CONFORMANCES-AVAILABLE-API

// REQUIRES: executable_test
// REQUIRES: OS=macosx

@available(macOS 10.15, *)
public protocol BaseProto {
  static var foo: String { get }
}

@available(macOS 10.15, *)
extension BaseProto {
  public static var foo: String { return "Base" }
}

@available(macOS 10.15.4, *)
public protocol DerivedProto: BaseProto {}

@available(macOS 10.15.4, *)
extension DerivedProto {
  public static var foo: String { return "Derived" }
}

@_spi(Private)
@available(macOS 10.15.4, *)
public protocol SecretDerivedProto: BaseProto {
}

@_spi(Private)
@available(macOS 10.15.4, *)
extension SecretDerivedProto {
  public static var foo: String { return "Secret" }
}

@available(macOS 10.15, *)
public struct NoSecrets: BaseProto {
  public init() {}
}

@available(macOS 10.15.4, *)
extension NoSecrets: DerivedProto {}

@available(macOS 10.15, *)
public struct HasSecrets: BaseProto {
  public init() {}
}

@_spi(Private)
@available(macOS 10.15.4, *)
extension HasSecrets: SecretDerivedProto {}

@available(macOS 10.15, *)
func doIt<T: BaseProto>(_: T) {
  print(T.foo)
}

// CHECK-CONFORMANCES-NOT-AVAILABLE:  Base
// CHECK-CONFORMANCES-AVAILABLE:      Derived
// CHECK-CONFORMANCES-AVAILABLE-API:  Base
doIt(NoSecrets())
// CHECK-CONFORMANCES-NOT-AVAILABLE:  Base
// CHECK-CONFORMANCES-AVAILABLE:      Secret
// CHECK-CONFORMANCES-AVAILABLE-API:  Secret
doIt(HasSecrets())
