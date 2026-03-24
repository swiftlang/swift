// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

// RUN: %target-swift-frontend -emit-module %t/Lib.swift -o %t/Lib.swiftmodule \
// RUN:   -swift-version 6

// RUN: %target-swift-frontend -emit-module %t/Client.swift -I %t \
// RUN:   -verify -verify-ignore-unrelated \
// RUN:   -swift-version 6 \
// RUN:   -enable-experimental-feature CheckImplementationOnly

// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

// RUN: %target-swift-frontend -emit-module %t/Lib.swift -o %t/Lib.swiftmodule \
// RUN:   -swift-version 6 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded

// RUN: %target-swift-frontend -emit-module %t/Client.swift -I %t \
// RUN:   -verify -verify-ignore-unrelated \
// RUN:   -swift-version 6 -target arm64-apple-none-macho \
// RUN:   -verify-additional-prefix embedded- \
// RUN:   -enable-experimental-feature Embedded \
// RUN:   -enable-experimental-feature CheckImplementationOnly

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_CheckImplementationOnly
// REQUIRES: embedded_stdlib_cross_compiling

//--- Lib.swift

public struct HiddenType {
    public init() {}
}

//--- Client.swift

@_implementationOnly import Lib

public protocol LocalType {}
extension HiddenType: LocalType {}

@frozen
public struct FrozenStruct {
  let propertyWithDefaultInit: LocalType = HiddenType() // expected-error {{struct 'HiddenType' cannot be used in a property initializer in a '@frozen' type because 'Lib' was imported implementation-only}}
  // expected-error @-1 {{initializer 'init()' cannot be used in a property initializer in a '@frozen' type because 'Lib' was imported implementation-only}}

  func funcWithDefaultArg(a: LocalType = HiddenType()) {} // expected-embedded-error {{struct 'HiddenType' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}
  // expected-embedded-error @-1 {{initializer 'init()' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}
}

public class PublicClass {
  let propertyWithDefaultInit: LocalType = HiddenType() // expected-embedded-error {{struct 'HiddenType' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}
  // expected-embedded-error @-1 {{initializer 'init()' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}

  func funcWithDefaultArg(a: LocalType = HiddenType()) {} // expected-embedded-error {{struct 'HiddenType' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}
  // expected-embedded-error @-1 {{initializer 'init()' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}

  @export(interface)
  func funcWithDefaultArgExportInterface(a: LocalType = HiddenType()) {}
}

internal struct InternalStruct {
  let propertyWithDefaultInit: LocalType = HiddenType() // expected-embedded-error {{struct 'HiddenType' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}
  // expected-embedded-error @-1 {{initializer 'init()' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}

  func funcWithDefaultArg(a: LocalType = HiddenType()) {} // expected-embedded-error {{struct 'HiddenType' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}
  // expected-embedded-error @-1 {{initializer 'init()' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}

  @export(interface)
  func funcWithDefaultArgExportInterface(a: LocalType = HiddenType()) {}
}
