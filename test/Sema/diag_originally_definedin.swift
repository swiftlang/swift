// RUN: %target-typecheck-verify-swift
// REQUIRES: OS=macosx

@_originallyDefinedIn(module: "original", OSX 10.13) // expected-error {{need @available attribute for @_originallyDefinedIn}}
public func foo() {}

@available(macOS 10.13, *)
@_originallyDefinedIn(module: "original", OSX 10.12) // expected-error {{moved version from @_originallyDefinedIn must after introduced OS version}}
public class C {
  @_originallyDefinedIn(module: "original", OSX 10.13) // expected-error {{@_originallyDefinedIn is only applicable to top-level decl}}
  public func foo() {}
}
