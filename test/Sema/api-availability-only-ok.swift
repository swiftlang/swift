/// Derived from api-availability-only with the errors fixed to check that the
/// generated module interface can be built.

// RUN: %empty-directory(%t)

// RUN: %swiftc_driver -emit-module %s -target %target-cpu-apple-macosx10.15 -emit-module-interface -emit-module-interface-path %t/main.swiftinterface -enable-library-evolution -Xfrontend -check-api-availability-only -verify-emitted-module-interface
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/main.swiftinterface

// REQUIRES: OS=macosx

@available(macOS 11.0, *)
public protocol NewProto {}

@available(macOS 11.0, *)
public func newFunc() {}

@available(macOS 11.0, *)
public func apiFunc(s : NewProto) {
  let _: NewProto
  newFunc()
}

@available(macOS 11.0, *)
@usableFromInline func usableFromInline(s : NewProto) {
  let _: NewProto
  newFunc()
}

@available(macOS 11.0, *)
@inlinable func inlinable(s : NewProto) {
  let _: NewProto
  newFunc()
}

@available(macOS 11.0, *)
@_spi(SomeSPI) public func spiFunc(s : NewProto) {
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

public struct Struct {
  internal var internalVar: NewProto
  private var privateVar: NewProto
  fileprivate var fileprivateVar: NewProto

  @available(macOS 11.0, *)
  public typealias PubTA = NewProto
  private typealias PrivateTA = NewProto

  @available(macOS 11.0, *)
  public func apiFunc(s : NewProto) {
    let _: NewProto
    newFunc()
  }
  
  @available(macOS 11.0, *)
  @usableFromInline func usableFromInline(s : NewProto) {
    let _: NewProto
    newFunc()
  }
  
  @available(macOS 11.0, *)
  @inlinable func inlinable(s : NewProto) {
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
}
