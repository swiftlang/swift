// RUN: %target-typecheck-verify-swift

@_originallyDefinedIn(module: "foo", OSX 13.13)
func foo() {}

@_originallyDefinedIn(modulename: "foo", OSX 13.13) // expected-error {{expected 'module: "original"' in the first argument to @_originallyDefinedIn}}
func foo1() {}

@_originallyDefinedIn(module: "foo", OSX 13.13.3) // expected-warning {{@_originallyDefinedIn only uses major and minor version number}}
class ToplevelClass {}

@_originallyDefinedIn(module: "foo") // expected-error {{expected at least one platform version in @_originallyDefinedIn}}
class ToplevelClass1 {}

@_originallyDefinedIn(OSX 13.13.3) // expected-error {{expected 'module: "original"' in the first argument to @_originallyDefinedIn}}
class ToplevelClass2 {}

@_originallyDefinedIn(module: "foo", // expected-error {{expected at least one platform version in @_originallyDefinedIn}}
class ToplevelClass3 {}

@_originallyDefinedIn(module: "foo", * 13.13) // expected-warning {{* as platform name has no effect}} expected-error {{expected at least one platform version in @_originallyDefinedIn}}
@_originallyDefinedIn(module: "foo", OSX 13.13, iOS 7.0)
@_originallyDefinedIn(module: "foo", OSX 13.14, * 7.0) // expected-warning {{* as platform name has no effect}} expected-error {{duplicate version number for platform OSX}}
class ToplevelClass4 {
	@_originallyDefinedIn(module: "foo", OSX 13.13) // expected-error {{'@_originallyDefinedIn' attribute cannot be applied to this declaration}}
	subscript(index: Int) -> Int {
        get { return 1 }
        set(newValue) {}
	}
}
