// RUN: %target-swift-frontend %s -emit-module -emit-module-interface-path %t/test.swiftmodule \
// RUN:   -swift-version 5 \
// RUN:   -o /dev/null \
// RUN:   -enable-library-evolution \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -enable-builtin-module \
// RUN:   -module-name test \
// RUN:   -define-availability "Span 0.1:macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, visionOS 9999" \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature AddressableParameters

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_AddressableParameters

// Test diagnostic output for interesting corner cases. Similar to semantics.swift, but this tests corner cases in the
// implementation as opposed to basic language rules.

import Builtin

public struct NE : ~Escapable {
}

public struct NEImmortal: ~Escapable {
  @_lifetime(immortal)
  public init() {}
}

class C {}

// Test that we don't implicitly try to create a keypath getter, since
// ~Escapable types are not yet supported by keypaths.
public struct ImplicitAccessors {
  let c: C

  public var neComputed: NEImmortal {
    get {
      NEImmortal()
    }
    set {
    }
  }
}

public struct NoncopyableImplicitAccessors : ~Copyable & ~Escapable {
  public var ne: NE

  public var neComputedBorrow: NE {
    @_lifetime(borrow self)
    get { ne }

    @_lifetime(&self)
    set {
      ne = newValue
    }
  }
}
