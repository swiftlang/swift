// RUN: %target-typecheck-verify-swift \
// RUN:   -define-availability "_myProject 1.0:macOS 10.10"
// REQUIRES: OS=macosx

@_originallyDefinedIn(module: "original", OSX 10.13) // expected-error {{'@_originallyDefinedIn' requires that 'foo()' have explicit availability for macOS}}
public func foo() {}

@available(macOS 10.13, *)
@_originallyDefinedIn(module: "original", OSX 10.12) // expected-error {{symbols are moved to the current module before they were available in the OSs}}
public class C {
  @_originallyDefinedIn(module: "original", OSX 10.13) // expected-error {{'@_originallyDefinedIn' is only applicable to top-level decl}}
  public func foo() {}
}

@available(macOS 10.13, *)
@_originallyDefinedIn(module: "original", OSX 10.13)
public class D {}

@_originallyDefinedIn(module: "original", macOS) // expected-error {{expected version number in '@_originallyDefinedIn' attribute}}
public func missingVersion() {}

@_originallyDefinedIn(module: "original", macOS 0) // expected-warning {{expected version number in '@_originallyDefinedIn' attribute; this is an error in the Swift 6 language mode}}
public func versionZero() {}

@available(macOS 10.9, *)
@_originallyDefinedIn(module: "original", _myProject 2.0) // expected-error {{reference to undefined version '2.0' for availability macro '_myProject'}}
public func macroVersioned() {}

// Fallback to the default diagnostic when the macro is unknown.
@available(macOS 10.9, *)
@_originallyDefinedIn(module: "original", _unknownMacro) // expected-warning {{unknown platform '_unknownMacro' for attribute '@_originallyDefinedIn'}}
// expected-error@-1 {{expected version number in '@_originallyDefinedIn' attribute}}
public func macroUnknown() {}

@available(macOS 10.9, *)
@_originallyDefinedIn(module: "original", macos 10.13) // expected-warning {{unknown platform 'macos' for attribute '@_originallyDefinedIn'; did you mean 'macOS'?}} {{43-48=macOS}}
public func incorrectPlatformCase() {}

@available(macOS 10.9, *)
@_originallyDefinedIn(module: "original", mscos 10.13) // expected-warning {{unknown platform 'mscos' for attribute '@_originallyDefinedIn'; did you mean 'macOS'?}} {{43-48=macOS}}
public func incorrectPlatformSimilar1() {}

@available(macOS 10.9, *)
@_originallyDefinedIn(module: "original", macoss 10.13) // expected-warning {{unknown platform 'macoss' for attribute '@_originallyDefinedIn'; did you mean 'macOS'?}} {{43-49=macOS}}
public func incorrectPlatformSimilar2() {}

@available(macOS 10.9, *)
@_originallyDefinedIn(module: "original", mac 10.13) // expected-warning {{unknown platform 'mac' for attribute '@_originallyDefinedIn'; did you mean 'macOS'?}} {{43-46=macOS}}
public func incorrectPlatformSimilar3() {}

@available(macOS 10.9, *)
@_originallyDefinedIn(module: "original", notValid 10.13) // expected-warning {{unknown platform 'notValid' for attribute '@_originallyDefinedIn'}} {{none}}
public func incorrectPlatformNotSimilar() {}

@available(macOS 10.9, *)
@_originallyDefinedIn(module: "original", swift 5.1) // expected-warning {{unknown platform 'swift' for attribute '@_originallyDefinedIn'}}
public func swiftVersionMacro() {}
