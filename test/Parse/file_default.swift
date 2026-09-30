// RUN: %target-typecheck-verify-swift -enable-experimental-feature DefaultIsolationPerFile

// REQUIRES: swift_feature_DefaultIsolationPerFile

// REQUIRES: concurrency

default @MainActor
// expected-note@-1:1 {{file-level default isolation previously declared here}}

nonisolated func foo() {}
foo(); default nonisolated; foo()
// expected-error@-1:8 {{invalid redeclaration of file-level default isolation}}

default @diagnose(StrictMemorySafety, as: error)

default @diagnose(StrictMemorySafety, as: warning); default @diagnose(StrictMemorySafety, as: error)

default @diagnose(StrictMemorySafety, as: error) func frog() {}
// expected-error@-1:49 {{consecutive statements on a line must be separated by ';'}}{{49-49=;}}

default func bizarre() {}
// expected-error@-1:9 {{expected '@MainActor', 'nonisolated', '@available', or '@diagnose' after 'default'}}

default foo
// expected-error@-1:9 {{expected '@MainActor', 'nonisolated', '@available', or '@diagnose' after 'default'}}

default `nonisolated`
// expected-error@-1:9 {{expected '@MainActor', 'nonisolated', '@available', or '@diagnose' after 'default'}}

default =
// expected-error@-1:9 {{expected '@MainActor', 'nonisolated', '@available', or '@diagnose' after 'default'}}

default @foo
// expected-error@-1:10 {{cannot find type 'foo' in scope}}
// expected-note@-2:9 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

// Looking on the next line would risk cascading errors.
default
// expected-error@-1:8 {{expected '@MainActor', 'nonisolated', '@available', or '@diagnose' after 'default'}}
nonisolated func bar() {}

default
// expected-error@-1:8 {{expected '@MainActor', 'nonisolated', '@available', or '@diagnose' after 'default'}}
@available(*, deprecated, message: "no more baz!")
func baz() {}

default
// expected-error@-1:8 {{expected '@MainActor', 'nonisolated', '@available', or '@diagnose' after 'default'}}

// TODO: in the future, consider a more nuanced recovery for ':' in file level default?

default: @MainActor
// expected-error@-1:1 {{'default' label can only appear inside a 'switch' statement}} {{none}}

// An example of why we probably can't just look for '@':
func braceMismatch() {
  switch Bool.random() {
  case true: break
  case false: break
  }} // Accidentally close the switch early...
  default: @MainActor struct Bar {}
  // expected-error@-1:3 {{'default' label can only appear inside a 'switch' statement}} {{none}}

private default @diagnose(StrictMemorySafety, as: error)
// expected-error@-1:1 {{attribute cannot be attached to a file-level default}}

default @inlinable
// expected-error@-1:9 {{'@inlinable' is not a valid file-level default}}
// expected-note@-2:9 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

default @backDeployed(before: macOS 13.0)
// expected-error@-1:10 {{'@backDeployed' is not a valid file-level default}}
// expected-note@-2:10 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

default @backDeployed(before: macOS 13.0, iOS 16.0)
// expected-error@-1:10 {{'@backDeployed' is not a valid file-level default}}
// expected-note@-2:10 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

default @_originallyDefinedIn(module: "Other", macOS 13.0)
// expected-error@-1:10 {{'@_originallyDefinedIn' is not a valid file-level default}}
// expected-note@-2:10 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

default @_originallyDefinedIn(module: "Other", macOS 13.0, iOS 16.0)
// expected-error@-1:10 {{'@_originallyDefinedIn' is not a valid file-level default}}
// expected-note@-2:10 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

@globalActor
actor MyActor { // expected-note@:7 {{'MyActor' declared here}}
  static let shared = MyActor()
  default @MyActor
  // expected-error@-1:3 {{declaration is only valid at file scope}}
  // TODO: we don't diagnose nested 'default' misuse since the request doesn't see it.
  // Fixing that would also fix diagnosing in files with only 'default'...
}

default @MyActor
// expected-error@-1:9 {{global actor 'MyActor' is not a valid file-level default}}
// expected-note@-2:9 {{file-level default isolation must be '@MainActor' or 'nonisolated'}}

do {
  default @MainActor // expected-error@:3 {{declaration is only valid at file scope}}
  default: @MainActor // expected-error@:3 {{'default' label can only appear inside a 'switch' statement}}
} // expected-error@:1 {{expected declaration}}

func test() {
  default @MainActor // expected-error@:3 {{declaration is only valid at file scope}}
  default: @MainActor // expected-error@:3 {{'default' label can only appear inside a 'switch' statement}}
} // expected-error@:1 {{expected declaration}}

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
  // expected-error@-1:10 {{expected '@MainActor', 'nonisolated', '@available', or '@diagnose' after 'default'}}
  // expected-error@-2:11 {{consecutive declarations on a line must be separated by ';'}}{{11-11=;}}

  default: nonisolated func lobster() {}
  // expected-error@-1:10 {{expected '@MainActor', 'nonisolated', '@available', or '@diagnose' after 'default'}}
  // expected-error@-2:11 {{consecutive declarations on a line must be separated by ';'}}{{11-11=;}}
}

do {
  @objc default @MainActor
  // expected-error@-1:9 {{declaration is only valid at file scope}}
  // expected-error@-2:4 {{attribute cannot be attached to a file-level default}}
}

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
