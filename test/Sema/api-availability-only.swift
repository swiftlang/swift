/// Test that -check-api-availability-only skips what is expected while checking
/// the module API and SPI.

// RUN: %target-typecheck-verify-swift -module-name MyModule -target x86_64-apple-macosx10.15 -check-api-availability-only -enable-library-evolution

// REQUIRES: OS=macosx

@available(macOS 11.0, *)
public protocol NewProto {}

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

// expected-note @+1 3 {{add @available attribute to enclosing}}
@inlinable func inlinable(s : NewProto) { // expected-error {{'NewProto' is only available in macOS 11.0 or newer}}

  // expected-note @+1 {{add 'if #available' version check}}
  let _: NewProto // expected-error {{'NewProto' is only available in macOS 11.0 or newer}}

  // expected-note @+1 {{add 'if #available' version check}}
  newFunc() // expected-error {{'newFunc()' is only available in macOS 11.0 or newer}}
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

// expected-note @+1 7 {{add @available attribute to enclosing struct}}
public struct Struct {
  public var publicVar: NewProto // expected-error {{'NewProto' is only available in macOS 11.0 or newer}}
  internal var internalVar: NewProto
  private var privateVar: NewProto
  fileprivate var fileprivateVar: NewProto

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
