// RUN: %target-typecheck-verify-swift
// REQUIRES: OS=macosx

@_originallyDefinedIn(module: "foo", OSX 13.13) // expected-error {{'@_originallyDefinedIn' requires that 'foo()' have explicit availability for macOS}}
public func foo() {}

@_originallyDefinedIn(modulename: "foo", OSX 13.13) // expected-error {{expected 'module: "original"' in the first argument to '@_originallyDefinedIn'}}
public func foo1() {}

@_originallyDefinedIn(module: "foo", OSX 13.13.3) // expected-warning {{'@_originallyDefinedIn' only uses major and minor version number}}
// expected-error@-1 {{'@_originallyDefinedIn' requires that 'ToplevelClass' have explicit availability for macOS}}
public class ToplevelClass {}

@_originallyDefinedIn(module: "foo") // expected-error {{expected at least one platform version in '@_originallyDefinedIn' attribute}}
public class ToplevelClass1 {}

@_originallyDefinedIn(OSX 13.13.3) // expected-error {{expected 'module: "original"' in the first argument to '@_originallyDefinedIn'}}
public class ToplevelClass2 {}

@_originallyDefinedIn(module: "foo",
public class ToplevelClass3 {} // expected-error {{expected platform in '@_originallyDefinedIn' attribute}}

@available(OSX 13.10, *)
@_originallyDefinedIn(module: "foo", * 13.13) // expected-warning {{* as platform name has no effect}} expected-error {{expected at least one platform version in '@_originallyDefinedIn' attribute}}
@_originallyDefinedIn(module: "foo", OSX 13.13, iOS 7.0)
@_originallyDefinedIn(module: "foo", OSX 13.14, * 7.0) // expected-warning {{* as platform name has no effect}} expected-error {{'@_originallyDefinedIn' contains multiple versions for macOS}}
public class ToplevelClass4 {
	@_originallyDefinedIn(module: "foo", OSX 13.13) // expected-error {{'@_originallyDefinedIn' attribute cannot be applied to this declaration}}
	subscript(index: Int) -> Int {
        get { return 1 }
        set(newValue) {}
	}
}

@available(OSX 13.10, *)
@_originallyDefinedIn(module: "foo", OSX 13.13) // expected-warning {{'@_originallyDefinedIn' does not have any effect on internal declarations}}
@_originallyDefinedIn(module: "foo", iOS 7.0)
internal class ToplevelClass5 {}

@available(OSX 13.10, *)
@_originallyDefinedIn(module: "foo", OSX 13.13) // expected-warning {{'@_originallyDefinedIn' does not have any effect on private declarations}}
@_originallyDefinedIn(module: "foo", iOS 7.0)
private class ToplevelClass6 {}

@available(OSX 13.10, *)
@_originallyDefinedIn(module: "foo", OSX 13.13) // expected-warning {{'@_originallyDefinedIn' does not have any effect on fileprivate declarations}}
@_originallyDefinedIn(module: "foo", iOS 7.0)
fileprivate class ToplevelClass7 {}

@available(OSX 13.10, *)
@_originallyDefinedIn(module: "foo", OSX 13.13, iOS 7.0) // expected-warning {{'@_originallyDefinedIn' does not have any effect on internal declarations}}
internal class ToplevelClass8 {}
