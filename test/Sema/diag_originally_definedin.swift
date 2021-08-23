// RUN: %target-typecheck-verify-swift \
// RUN:   -define-availability "_myProject 1.0:macOS 10.10"
// REQUIRES: OS=macosx

@_originallyDefinedIn(module: "original", OSX 10.13) // expected-error {{need @available attribute for @_originallyDefinedIn}}
public func foo() {}

@available(macOS 10.13, *)
@_originallyDefinedIn(module: "original", OSX 10.12) // expected-error {{moved version from @_originallyDefinedIn must after introduced OS version}}
public class C {
  @_originallyDefinedIn(module: "original", OSX 10.13) // expected-error {{@_originallyDefinedIn is only applicable to top-level decl}}
  public func foo() {}
}

@available(macOS 10.9, *)
@_originallyDefinedIn(module: "original", _myProject 2.0) // expected-error {{expected at least one platform version in @_originallyDefinedIn}}
// expected-error @-1 {{reference to undefined version '2.0' for availability macro '_myProject'}}
public func macroVersionned() {}

// Fallback to the default diagnostic when the macro is unknown.
@available(macOS 10.9, *)
@_originallyDefinedIn(module: "original", _unknownMacro) // expected-error {{expected at least one platform version in @_originallyDefinedIn}}
public func macroUnknown() {}
