// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

import COM

@com(interface: "45000000-0000-0000-0000-000000000001")
public protocol IItem {
}

@com(interface: "45000000-0000-0000-0000-000000000002")
public protocol IProvider {
  func GetItem(_ item: UnsafeMutablePointer<(any IItem)?>?) -> CInt
}

@c(accept_item)
func accept(item: any IItem) {
}

@c(fill_item)
func fill(item: UnsafeMutablePointer<(any IItem)?>?) {
}

func getItem(_ provider: borrowing any IProvider) -> (any IItem)? {
  var item: (any IItem)?
  _ = provider.GetItem(&item)
  return item
}

@com(implementation: "45000000-0000-0000-0000-000000000003")
open class CObject: IProvider {
  public init() {
  }

  open func GetItem(_ item: UnsafeMutablePointer<(any IItem)?>?) -> CInt {
    0
  }
}

protocol SwiftItem {
}

@com(interface: "45000000-0000-0000-0000-000000000004")
protocol IInvalid {
  func GetSwiftItem(_ item: UnsafeMutablePointer<(any SwiftItem)?>?) -> CInt
  // expected-error@-1 {{type 'UnsafeMutablePointer<(any SwiftItem)?>?' of COM interface requirement 'GetSwiftItem' cannot be represented in C}}
}
