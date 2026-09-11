// RUN: %target-typecheck-verify-swift -enable-experimental-feature DefaultIsolationPerFile

// REQUIRES: swift_feature_DefaultIsolationPerFile

// REQUIRES: concurrency

default @MainActor
// expected-note@-1:1 2 {{file-level default isolation previously declared here}}

nonisolated func foo() {}
foo(); default nonisolated; foo()
// expected-error@-1:8 {{invalid redeclaration of file-level default isolation}}

default @diagnose(StrictMemorySafety, as: error)

default @diagnose(StrictMemorySafety, as: warning); default @diagnose(StrictMemorySafety, as: error)

// MARK: - invalid specifiers

default func bizarre() {}
// expected-error@-1:9 {{expected '@MainActor', 'nonisolated', '@available', or '@diagnose' after 'default'}}

default foo
// expected-error@-1:9 {{expected '@MainActor', 'nonisolated', '@available', or '@diagnose' after 'default'}}

default foo()
// expected-error@-1:9 {{expected '@MainActor', 'nonisolated', '@available', or '@diagnose' after 'default'}}

default @foo
// expected-error@-1:10 {{cannot find type 'foo' in scope}}
// expected-note@-2:9 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

default @foo()
// expected-error@-1:10 {{cannot find type 'foo' in scope}}
// expected-note@-2:9 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

private default @diagnose(StrictMemorySafety, as: error)
// expected-error@-1:1 {{attribute cannot be attached to a 'default' declaration}}

// TODO: can we do better in this case? is it worth it?
private default: @diagnose(StrictMemorySafety, as: error)
// expected-error@-1:1 {{expected expression}}
// expected-error@-2:16 {{unexpected ':' in file-level default}}{{16-17=}}

default `nonisolated`
// expected-error@-1:9 {{expected '@MainActor', 'nonisolated', '@available', or '@diagnose' after 'default'}}

default
// expected-error@-1:8 {{expected '@MainActor', 'nonisolated', '@available', or '@diagnose' after 'default'}}

// MARK: - a key/value spelling recovers by dropping the separator

default: @available(iOS 1.0, macOS 1.0, tvOS 1.0, watchOS 1.0, *)
// expected-error@-1:8 {{unexpected ':' in file-level default}}{{8-9=}}

default: @foo
// expected-error@-1:8 {{unexpected ':' in file-level default}}{{8-9=}}
// expected-error@-2:11 {{cannot find type 'foo' in scope}}
// expected-note@-3:10 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

default: @foo()
// expected-error@-1:8 {{unexpected ':' in file-level default}}{{8-9=}}
// expected-error@-2:11 {{cannot find type 'foo' in scope}}
// expected-note@-3:10 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

// Should be a redeclaration after recovery.
default: nonisolated
// expected-error@-1:8 {{unexpected ':' in file-level default}}{{8-9=}}
// expected-error@-2:1 {{invalid redeclaration of file-level default isolation}}

default = @available(iOS 1.0, macOS 1.0, tvOS 1.0, watchOS 1.0, *)
// expected-error@-1:9 {{unexpected '=' in file-level default}}{{9-11=}}

default = @diagnose(StrictMemorySafety, as: warning, reason: ":3")
// expected-error@-1:9 {{unexpected '=' in file-level default}}{{9-11=}}

default = foo
// expected-error@-1:9 {{unexpected '=' in file-level default}}{{9-11=}}
// expected-error@-2:11 {{expected '@MainActor', 'nonisolated', '@available', or '@diagnose' after 'default'}}

default = @foo
// expected-error@-1:9 {{unexpected '=' in file-level default}}{{9-11=}}
// expected-error@-2:12 {{cannot find type 'foo' in scope}}
// expected-note@-3:11 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

// MARK: - a ':' before something that can't be a specifier is a 'switch' label

default: foo
// expected-error@-1:1 {{'default' label can only appear inside a 'switch' statement}} {{none}}

default: foo()
// expected-error@-1:1 {{'default' label can only appear inside a 'switch' statement}} {{none}}

// MARK: - unsupported attributes and modifiers

default async
// expected-error@-1:9 {{expected '@MainActor', 'nonisolated', '@available', or '@diagnose' after 'default'}}

default break
// expected-error@-1:9 {{expected '@MainActor', 'nonisolated', '@available', or '@diagnose' after 'default'}}
// expected-error@-2:9 {{'break' is only allowed inside a loop, if, do, or switch}}

default @inlinable
// expected-error@-1:9 {{'@inlinable' is not valid in a 'default' declaration}}
// expected-note@-2:9 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

default @backDeployed(before: macOS 13.0)
// expected-error@-1:10 {{'@backDeployed' is not valid in a 'default' declaration}}
// expected-note@-2:10 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

default @backDeployed(before: macOS 13.0, iOS 16.0)
// expected-error@-1:10 {{'@backDeployed' is not valid in a 'default' declaration}}
// expected-note@-2:10 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

default @_originallyDefinedIn(module: "Other", macOS 13.0)
// expected-error@-1:10 {{'@_originallyDefinedIn' is not valid in a 'default' declaration}}
// expected-note@-2:10 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

default @_originallyDefinedIn(module: "Other", macOS 13.0, iOS 16.0)
// expected-error@-1:10 {{'@_originallyDefinedIn' is not valid in a 'default' declaration}}
// expected-note@-2:10 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

// MARK: - global actors other than MainActor get a tailored diagnostic

@globalActor
actor MyActor { // expected-note@:7 {{'MyActor' declared here}}
  static let shared = MyActor()
  default @MyActor
  // expected-error@-1:3 {{declaration is only valid at file scope}}
  // TODO: we don't diagnose nested 'default' misuse since the request doesn't see it.
  // Fixing that would also fix diagnosing in files with only 'default'...
}

default @MyActor
// expected-error@-1:9 {{global actor 'MyActor' is not valid in a 'default' declaration}}
// expected-note@-2:9 {{file-level default isolation must be '@MainActor' or 'nonisolated'}}

// MARK: - only valid at file scope

do {
  default @MainActor // expected-error@:3 {{declaration is only valid at file scope}}
  default: @MainActor // expected-error@:10 {{unexpected ':' in file-level default}}{{10-11=}}
  // expected-error@-1:3 {{declaration is only valid at file scope}}
  default = @MainActor // expected-error@:11 {{unexpected '=' in file-level default}}{{11-13=}}
  // expected-error@-1:3 {{declaration is only valid at file scope}}
}

func test() {
  default @MainActor // expected-error@:3 {{declaration is only valid at file scope}}

  default: @MainActor // expected-error@:3 {{'default' label can only appear inside a 'switch' statement}}
  // expected-error@-1:12 {{attribute cannot be attached to a 'default' declaration}}

  default = @MainActor // expected-error@:11 {{unexpected '=' in file-level default}}{{11-13=}}
  // expected-error@-1:3 {{declaration is only valid at file scope}}
}

struct S {
  var x: Int {
    default @MainActor // expected-error@:5 {{declaration is only valid at file scope}}
    default: nonisolated // expected-error@:5 {{'default' label can only appear inside a 'switch' statement}}
  }

  default @MainActor func lion() {}
  // expected-error@-1:3 {{declaration is only valid at file scope}}
  // expected-error@-2:21 {{consecutive declarations on a line must be separated by ';'}}{{21-21=;}}

  default nonisolated subscript(a: Int) -> Bool { false }
  // expected-error@-1:3 {{declaration is only valid at file scope}}
  // expected-error@-2:22 {{consecutive declarations on a line must be separated by ';'}}{{22-22=;}}

  default: func lamb() {}
  // expected-error@-1:3 {{'default' label can only appear inside a 'switch' statement}}
  // expected-error@-2:11 {{consecutive declarations on a line must be separated by ';'}}{{11-11=;}}

  private default: @diagnose(StrictMemorySafety, as: error)
  // expected-error@-1:3 {{attribute cannot be attached to a 'default' declaration}}
  // expected-error@-2:11 {{declaration is only valid at file scope}}
  // expected-error@-3:18 {{unexpected ':' in file-level default}}{{18-19=}}

  private default: break
  // expected-error@-1:11 {{'default' label can only appear inside a 'switch' statement}}
}

do {
  @objc default @MainActor
  // expected-error@-1:9 {{declaration is only valid at file scope}}
  // expected-error@-2:4 {{attribute cannot be attached to a 'default' declaration}}
}

// MARK: - inside a switch

switch 3 {
case 3: print("3")
default nonisolated
// expected-error@-1:9 {{expected ':' after 'default'}}
// expected-error@-2:9 {{cannot find 'nonisolated' in scope}}
}

switch 4 {
case 4: print("4")
default: nonisolated
// expected-error@-1:10 {{cannot find 'nonisolated' in scope}}
}

// TODO: maybe these could be better...

switch 5 {
case 5: print("5");
default @MainActor
// expected-error@-1:9 {{expected ':' after 'default'}}
} // expected-error {{expected declaration}}

switch 6 {
case 6: print("6");
default: @MainActor
} // expected-error {{expected declaration}}
