// RUN: %target-swift-frontend %s -emit-module -emit-module-interface-path %t/test.swiftmodule \
// RUN:   -o /dev/null \
// RUN:   -enable-library-evolution \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -enable-builtin-module \
// RUN:   -module-name test \
// RUN:   -define-availability "Span 0.1:macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, visionOS 9999" \
// RUN:   -enable-experimental-feature LifetimeDependence \
// RUN:   -enable-experimental-feature AddressableParameters

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_AddressableParameters

// Test diagnostic output for interesting corner cases. Similar to semantics.swift, but this tests corner cases in the
// implementation as opposed to basic language rules.

import Builtin

public struct NE : ~Escapable {
}

public struct NEImmortal: ~Escapable {
  @lifetime(immortal)
  public init() {}
}

class C {}

// Test diagnostics on keypath getter.
//
// FIXME: rdar://150073405 ([SILGen] support synthesized _modify on top of borrowed getters with library evolution)
//
// This produces the error:
// <unknown>:0: error: unexpected error produced: lifetime-dependent value returned by generated thunk
// '$s4test17ImplicitAccessorsV10neComputedAA10NEImmortalVvpACTK'
//
// Since this error has no source file, we can't verify the diagnostic!
/*
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
 */

public struct NoncopyableImplicitAccessors : ~Copyable & ~Escapable {
  public var ne: NE

  public var neComputedBorrow: NE {
    // expected-error @-1{{lifetime-dependent value returned by generated accessor '_modify'}}
    // expected-note  @-2{{it depends on this scoped access to variable 'self'}}
    @lifetime(borrow self)
    get { ne }

    @lifetime(&self)
    set {
      ne = newValue
    }
  }
}
