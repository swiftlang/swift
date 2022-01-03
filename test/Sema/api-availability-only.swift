/// Test that -check-api-availability-only skips what is expected while checking
/// the module API and SPI.

// RUN: %target-typecheck-verify-swift -module-name MyModule -target %target-cpu-apple-macosx10.14 -check-api-availability-only -enable-library-evolution

/// The flag -check-api-availability-only should reject builds going up to IR and further.
// RUN: not %target-build-swift -emit-executable %s -g -o %t -emit-module -Xfrontend -check-api-availability-only 2>&1 | %FileCheck %s
// CHECK: the flag -check-api-availability-only does not support emitting IR

// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64

@available(macOS 11.0, *)
public protocol NewProto {}

@available(macOS 11.0, *)
public struct NewStruct {
    public init() {}
}

@available(macOS 11.0, *)
public func newFunc() {}

// expected-note @+1 {{add @available attribute to enclosing}}
public func apiFunc(s : NewProto) { // expected-error {{'NewProto' is only available in macOS 11.0 or newer}}
  let _: NewProto
  newFunc()
}

// expected-note @+1 {{add @available attribute to enclosing}}
@usableFromInline func usableFromInline(s : NewProto) { // expected-error {{'NewProto' is only available in macOS 11.0 or newer}}
  let _: NewProto
  newFunc()
}

// expected-note @+1 6 {{add @available attribute to enclosing}}
@inlinable func inlinable(s : NewProto) { // expected-error {{'NewProto' is only available in macOS 11.0 or newer}}

  // expected-note @+1 {{add 'if #available' version check}}
  let _: NewProto // expected-error {{'NewProto' is only available in macOS 11.0 or newer}}

  // expected-note @+1 {{add 'if #available' version check}}
  newFunc() // expected-error {{'newFunc()' is only available in macOS 11.0 or newer}}

  // expected-note @+1 {{add 'if #available' version check}}
  let _ = NewStruct() // expected-error {{'NewStruct' is only available in macOS 11.0 or newer}}

  // expected-note @+2 {{add 'if #available' version check}}
  // expected-warning @+1 {{initialization of immutable value 'a' was never used}}
  let a = NewStruct() // expected-error {{'NewStruct' is only available in macOS 11.0 or newer}}

  // expected-note @+2 {{add 'if #available' version check}}
  // expected-warning @+1 {{result of 'NewStruct' initializer is unused}}
  NewStruct() // expected-error {{'NewStruct' is only available in macOS 11.0 or newer}}
}

// expected-note @+1 {{add @available attribute to enclosing}}
@_spi(SomeSPI) public func spiFunc(s : NewProto) { // expected-error {{'NewProto' is only available in macOS 11.0 or newer}}
  let _: NewProto
  newFunc()
}

func internalFunc(s : NewProto) {
  let _: NewProto
  newFunc()
}

private func privateFunc(s : NewProto) {
  let _: NewProto
  newFunc()
}

fileprivate func fileprivateFunc(s : NewProto) {
  let _: NewProto
  newFunc()
}

// expected-note @+1 8 {{add @available attribute to enclosing struct}}
public struct Struct {
  public var publicVar: NewProto // expected-error {{'NewProto' is only available in macOS 11.0 or newer}}
  internal var internalVar: NewProto
  private var privateVar: NewProto
  fileprivate var fileprivateVar: NewProto

  public var publicAssigned = NewStruct() // expected-error {{'NewStruct' is only available in macOS 11.0 or newer}}
  internal var internalAssigned = NewStruct()
  private var privateAssigned = NewStruct()
  fileprivate var fileprivateAssigned = NewStruct()

  // expected-note @+1 {{add @available attribute to enclosing}}
  public typealias PubTA = NewProto // expected-error {{'NewProto' is only available in macOS 11.0 or newer}}
  private typealias PrivateTA = NewProto

  // expected-note @+1 {{add @available attribute to enclosing}}
  public func apiFunc(s : NewProto) { // expected-error {{'NewProto' is only available in macOS 11.0 or newer}}
    let _: NewProto
    newFunc()
  }
  
  // expected-note @+1 {{add @available attribute to enclosing}}
  @usableFromInline func usableFromInline(s : NewProto) { // expected-error {{'NewProto' is only available in macOS 11.0 or newer}}
    let _: NewProto
    newFunc()
  }
  
  // expected-note @+1 3 {{add @available attribute to enclosing}}
  @inlinable func inlinable(s : NewProto) { // expected-error {{'NewProto' is only available in macOS 11.0 or newer}}
  
    // expected-note @+1 {{add 'if #available' version check}}
    let _: NewProto // expected-error {{'NewProto' is only available in macOS 11.0 or newer}}
  
    // expected-note @+1 {{add 'if #available' version check}}
    newFunc() // expected-error {{'newFunc()' is only available in macOS 11.0 or newer}}
  }
  
  func internalFunc(s : NewProto) {
    let _: NewProto
    newFunc()
  }
  
  private func privateFunc(s : NewProto) {
    let _: NewProto
    newFunc()
  }
  
  fileprivate func fileprivateFunc(s : NewProto) {
    let _: NewProto
    newFunc()
  }
}

// expected-note @+1 {{add @available attribute to enclosing}}
extension NewProto { // expected-error {{'NewProto' is only available in macOS 11.0 or newer}}
    public func foo() {}
}

func asyncFunc() async -> Bool {
    fatalError()
}

// expected-note @+1 {{add @available attribute to enclosing}}
public func publicAsyncFunc() async -> Bool { // expected-error {{concurrency is only available in macOS 10.15.0 or newer}}
    fatalError()
}

// expected-note @+1 {{add @available attribute to enclosing}}
@usableFromInline func usableFromInlineAsyncFunc() async -> Bool { // expected-error {{concurrency is only available in macOS 10.15.0 or newer}}
    fatalError()
}

actor InternalActor {
}

// expected-note @+1 {{add @available attribute to enclosing}}
public actor PublicActor { // expected-error {{concurrency is only available in macOS 10.15.0 or newer}}
}
