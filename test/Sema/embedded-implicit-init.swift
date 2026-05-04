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

public protocol LocalProto {}
extension HiddenType: LocalProto {}
public struct LocalType: LocalProto {}

@frozen
public struct FrozenStruct {
  let propertyWithDefaultInit: LocalProto = HiddenType() // expected-error {{struct 'HiddenType' cannot be used in a property initializer in a '@frozen' type because 'Lib' was imported implementation-only}}
  // expected-error @-1 {{initializer 'init()' cannot be used in a property initializer in a '@frozen' type because 'Lib' was imported implementation-only}}

  func funcWithDefaultArg(a: LocalProto = HiddenType()) {} // expected-embedded-error {{struct 'HiddenType' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}
  // expected-embedded-error @-1 {{initializer 'init()' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}
}

public class PublicClass {
  let propertyOk: LocalProto? = nil // OK

  let propertyWithDefaultInit: LocalProto = HiddenType() // expected-embedded-error {{struct 'HiddenType' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}
  // expected-embedded-error @-1 {{initializer 'init()' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}

  func funcWithDefaultArg(a: LocalProto = HiddenType()) {} // expected-embedded-error {{struct 'HiddenType' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}
  // expected-embedded-error @-1 {{initializer 'init()' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}

  @export(interface)
  func funcWithDefaultArgExportInterface(a: LocalProto = HiddenType()) {}
}

internal struct InternalStruct {
  let propertyWithDefaultInit: LocalProto = HiddenType() // expected-embedded-error {{struct 'HiddenType' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}
  // expected-embedded-error @-1 {{initializer 'init()' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}

  func funcWithDefaultArg(a: LocalProto = HiddenType()) {} // expected-embedded-error {{struct 'HiddenType' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}
  // expected-embedded-error @-1 {{initializer 'init()' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}

  @export(interface)
  func funcWithDefaultArgExportInterface(a: LocalProto = HiddenType()) {}
}

public class MissingBothIOIAndDeinit { // expected-embedded-note {{add a '@export(interface)' deinit to the parent class}}
  var prop: HiddenType = HiddenType()
  // expected-embedded-error @-1 {{cannot use struct 'HiddenType' in a stored property of a class without meeting the Embedded mode requirements; 'Lib' has been imported as implementation-only}}
  // expected-embedded-note @-2 {{mark the stored property '@_implementationOnly'}}
  // expected-embedded-error @-3 {{struct 'HiddenType' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}
  // expected-embedded-error @-4 {{initializer 'init()' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}
}

public class HasDeinitWithoutIOIProperty {
  var prop: HiddenType = HiddenType()
  // expected-embedded-error @-1 {{cannot use struct 'HiddenType' in a stored property of a class without meeting the Embedded mode requirements; 'Lib' has been imported as implementation-only}}
  // expected-embedded-note @-2 {{mark the stored property '@_implementationOnly'}}
  // expected-embedded-error @-3 {{struct 'HiddenType' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}
  // expected-embedded-error @-4 {{initializer 'init()' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}

  @export(interface)
  deinit {}
}

public class HasIOIPropertyWithoutDeinit { // expected-embedded-note {{add a '@export(interface)' deinit to the parent class}}
  @_implementationOnly var prop: HiddenType = HiddenType()
  // expected-embedded-error @-1 {{cannot use struct 'HiddenType' in a stored property of a class without meeting the Embedded mode requirements; 'Lib' has been imported as implementation-only}}
  // expected-embedded-error @-2 {{struct 'HiddenType' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}
  // expected-embedded-error @-3 {{initializer 'init()' cannot be used in an embedded function not marked '@export(interface)' because 'Lib' was imported implementation-only}}
}

public class HasBothIOIAndDeinit {
  @_implementationOnly var prop: HiddenType? = nil

  @export(interface)
  deinit {}

  @export(interface)
  func userOk() {
      let _ = prop
  }

  func userMissingAttribute() {
      let _ = prop // expected-embedded-error {{property 'prop' cannot be used in an embedded function not marked '@export(interface)' because 'prop' is marked '@_implementationOnly'}}
  }
}
