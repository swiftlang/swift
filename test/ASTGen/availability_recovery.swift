// Availability forms that used to crash ASTGen. Each is either accepted, or
// rejected with the same verdict as the C++ parser -- so the same expectations
// are checked against both parsers.

// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx13.0 \
// RUN:   -verify-additional-prefix astgen- \
// RUN:   -enable-experimental-feature ParserASTGen \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain
// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx13.0 \
// RUN:   -verify-additional-prefix cxx- \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain

// REQUIRES: OS=macosx
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_CustomAvailability

// A domain without a version is valid: a custom availability domain has no
// version. ASTGen used to require one and crash. Whether a version is required
// is decided after the domain resolves, in the type checker.
@available(EnabledDomain)
func availableInCustomDomain() {}

func customDomainQuery() {
  if #available(EnabledDomain) {}
  if #unavailable(EnabledDomain) {}
}

// A versioned domain that is missing its version is still diagnosed -- by the
// type checker rather than the parser, exactly as with the C++ parser.
func versionedDomainMissingVersion() {
  if #available(macOS) {} // expected-error {{expected version number}}
}

// A labeled argument cannot follow a shorthand specification.
@available(OSX 10.0, deprecated: 10.12)
// expected-error@-1 {{'deprecated' can't be combined with shorthand specification 'OSX 10.0'}}
// expected-note@-2 {{did you mean to specify an introduction version?}} {{15-15=, introduced:}}
// expected-cxx-error@-3 {{expected declaration}}
// expected-astgen-error@-4 {{must handle potential future platforms with '*'}}
func shorthandThenLabeled() {}

// A shorthand specification cannot follow the extended form.
@available(EnabledDomain, macOS 10.10, *)
// expected-error@-1 {{expected 'available' option such as 'unavailable', 'introduced', 'deprecated', 'obsoleted', 'message', or 'renamed'}}
// expected-cxx-error@-2 {{expected declaration}}
// expected-astgen-error@-3 {{expected 'available' option such as 'unavailable', 'introduced', 'deprecated', 'obsoleted', 'message', or 'renamed'}}
func extendedThenShorthand() {}

// Conflicting attribute kinds.
@available(macOS, deprecated, unavailable)
// expected-error@-1 {{'available' attribute cannot be both 'unavailable' and 'deprecated'}}
func conflictingKinds() {}

// A duplicated argument is a warning, not an error.
@available(*, unavailable, message: "a", message: "b")
// expected-warning@-1 {{'message' argument has already been specified}}
func duplicatedMessage() {}

// An unrecognized platform in a platform-version list is a warning, and the
// entry is dropped -- so this must not become an error in either parser.
@backDeployed(before: macos 12.0)
// expected-warning@-1 {{unknown platform 'macos' for attribute '@backDeployed'; did you mean 'macOS'?}} {{23-28=macOS}}
public func backDeployedPlatformTypo() {}

@backDeployed(before: NotAPlatform 12.0)
// expected-warning@-1 {{unknown platform 'NotAPlatform' for attribute '@backDeployed'}}
public func backDeployedUnknownPlatform() {}

// '*' names no platform here. It is reported, and unlike an unrecognized name it
// does not suppress the "needs a platform version" error.
@_originallyDefinedIn(module: "foo", * 13.13)
// expected-warning@-1 {{* as platform name has no effect in '@_originallyDefinedIn' attribute}}
// expected-error@-2 {{expected at least one platform version in '@_originallyDefinedIn' attribute}}
@available(macOS 13.10, *)
public class WildcardPlatform {}
