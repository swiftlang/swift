// RUN: %target-typecheck-verify-swift \
// RUN:   -define-availability "_iOS53Aligned:macOS 50.0, iOS 53.0" \
// RUN:   -define-availability "_iOS54Aligned:macOS 51.0, iOS 54.0" \
// RUN:   -define-availability "_iOS54:iOS 54.0" \
// RUN:   -define-availability "_macOS51_0:macOS 51.0" \
// RUN:   -define-availability "_myProject 1.0:macOS 51.0" \
// RUN:   -define-availability "_myProject 2.5:macOS 52.5"

// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature AvailabilityMacro='_iOS53Aligned:macOS 50.0, iOS 53.0' \
// RUN:   -enable-experimental-feature AvailabilityMacro="_iOS54Aligned:macOS 51.0, iOS 54.0" \
// RUN:   -enable-experimental-feature AvailabilityMacro='_iOS54:iOS 54.0' \
// RUN:   -enable-experimental-feature AvailabilityMacro="_macOS51_0:macOS 51.0" \
// RUN:   -enable-experimental-feature AvailabilityMacro='_myProject 1.0:macOS 51.0' \
// RUN:   -enable-experimental-feature AvailabilityMacro="_myProject 2.5:macOS 52.5"

// REQUIRES: OS=macosx

@available(_iOS53Aligned, *)
public func onMacOS50() {}

@available(_iOS54Aligned, *)
public func onMacOS51_0() {}

@available(_iOS54, _macOS51_0, tvOS 54.0, *)
public func composed() {}

@available(_iOS53Aligned, *)
@available(macOS, deprecated: 50)
public func onMacOSDeprecated() {}

@available(_myProject, *) // expected-error {{expected declaration}}
// expected-error @-1 {{reference to undefined version '0' for availability macro '_myProject'}}
public func onMyProject() {}

@available(_myProject 1.0, *)
public func onMyProjectV1() {}

@available(_myProject 2.5, *)
public func onMyProjectV2_5() {}

@available(_myProject 3.0, *)// expected-error {{expected declaration}}
// expected-error @-1 {{reference to undefined version '3.0' for availability macro '_myProject'}}
public func onMyProjectV3() {}

@available(_myProject 3_0, *)// expected-error {{expected declaration}}
// expected-error @-1 {{expected version number}}
public func brokenVersion() {}

@available(_unknownMacro, *) // expected-error {{expected declaration}}
// expected-error @-1 {{expected 'available' option such as 'unavailable', 'introduced', 'deprecated', 'obsoleted', 'message', or 'renamed'}}
public func unknownMacro() {}

@available(_iOS54) // expected-error {{must handle potential future platforms with '*'}}
public func noOtherOSes() {}

@available(_iOS53Aligned, *)
func client() {
  onMacOS50()
  onMacOS51_0() // expected-error {{is only available in macOS 51.0 or newer}}
  // expected-note @-1 {{add 'if #available' version check}}
  onMacOSDeprecated()

  if #available(_iOS54Aligned, *) {
    onMacOS51_0()
  }

  if #unavailable(_iOS54Aligned) {
    onMacOS51_0() // expected-error {{is only available in macOS 51.0 or newer}}
    // expected-note @-1 {{add 'if #available' version check}}
  } else {
    onMacOS51_0()
  }

  if #available(_unknownMacro, *) { } // expected-warning {{unrecognized platform name '_unknownMacro'}}
}

public func doIt(_ closure: () -> ()) {
  closure()
}

@inlinable
public func forbidMacrosInInlinableCode() {
  if #available(_iOS54Aligned, *) { } // expected-error {{availability macro cannot be used in an '@inlinable' function}}
  if #available(_iOS54, _macOS51_0, *) { } // expected-error {{availability macro cannot be used in an '@inlinable' function}}
  if #available(iOS 54.0, _macOS51_0, tvOS 54.0, *) { } // expected-error {{availability macro cannot be used in an '@inlinable' function}}
  if #unavailable(_iOS54Aligned) { } // expected-error {{availability macro cannot be used in an '@inlinable' function}}
  if #unavailable(_iOS54, _macOS51_0) { } // expected-error {{availability macro cannot be used in an '@inlinable' function}}
  if #unavailable(iOS 54.0, _macOS51_0, tvOS 54.0) { } // expected-error {{availability macro cannot be used in an '@inlinable' function}}
  doIt {
    if #available(_iOS54Aligned, *) { } // expected-error {{availability macro cannot be used in an '@inlinable' function}}
  }
}

@_alwaysEmitIntoClient
public func forbidMacrosInInlinableCode1() {
  if #available(_iOS54Aligned, *) { } // expected-error {{availability macro cannot be used in an '@_alwaysEmitIntoClient' function}}
  if #available(_iOS54, _macOS51_0, *) { } // expected-error {{availability macro cannot be used in an '@_alwaysEmitIntoClient' function}}
  if #available(iOS 54.0, _macOS51_0, tvOS 54.0, *) { } // expected-error {{availability macro cannot be used in an '@_alwaysEmitIntoClient' function}}
  if #unavailable(_iOS54Aligned) { } // expected-error {{availability macro cannot be used in an '@_alwaysEmitIntoClient' function}}
  if #unavailable(_iOS54, _macOS51_0) { } // expected-error {{availability macro cannot be used in an '@_alwaysEmitIntoClient' function}}
  if #unavailable(iOS 54.0, _macOS51_0, tvOS 54.0) { } // expected-error {{availability macro cannot be used in an '@_alwaysEmitIntoClient' function}}
  doIt {
    if #available(_iOS54Aligned, *) { } // expected-error {{availability macro cannot be used in an '@_alwaysEmitIntoClient' function}}
  }
}

@available(_iOS53Aligned, *)
@backDeployed(before: _iOS54Aligned)
public func forbidMacrosInInlinableCode2() {
  if #available(_iOS54Aligned, *) { } // expected-error {{availability macro cannot be used in a '@backDeployed' function}}
  if #available(_iOS54, _macOS51_0, *) { } // expected-error {{availability macro cannot be used in a '@backDeployed' function}}
  if #available(iOS 54.0, _macOS51_0, tvOS 54.0, *) { } // expected-error {{availability macro cannot be used in a '@backDeployed' function}}
  if #unavailable(_iOS54Aligned) { } // expected-error {{availability macro cannot be used in a '@backDeployed' function}}
  if #unavailable(_iOS54, _macOS51_0) { } // expected-error {{availability macro cannot be used in a '@backDeployed' function}}
  if #unavailable(iOS 54.0, _macOS51_0, tvOS 54.0) { } // expected-error {{availability macro cannot be used in a '@backDeployed' function}}
  doIt {
    if #available(_iOS54Aligned, *) { } // expected-error {{availability macro cannot be used in a '@backDeployed' function}}
  }
}
