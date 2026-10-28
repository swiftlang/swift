// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

@com(interface: "45000000-0000-0000-0000-000000000001")
protocol IItem {}

@com(interface: "45000000-0000-0000-0000-000000000002")
protocol IObject: AnyObject {}

@com(interface: "45000000-0000-0000-0000-000000000003")
protocol IDerived: IItem {}

typealias Item = any IItem

@com(interface: "45000000-0000-0000-0000-000000000004")
protocol IProvider {
  func direct(_ item: Item) -> Item
  func optional(_ item: Item?) -> Item?
  func derived(_ item: any IDerived) -> any IDerived
  func object(_ item: (any IObject)?) -> (any IObject)?
  func marker(_ item: any IItem & Sendable)
  func read(_ item: UnsafePointer<Item>)
  func write(_ item: UnsafeMutablePointer<Item?>?) -> CInt
}

@c(accept_item)
func accept(_ item: Item) {}

@c(fill_item)
func fill(_ item: UnsafeMutablePointer<Item?>?) {}

protocol SwiftItem {}
struct SwiftValue {}

@com(interface: "45000000-0000-0000-0000-000000000005")
protocol IInvalid {
  func native(_ item: any SwiftItem)
  // expected-error@-1 {{type 'any SwiftItem' of COM interface requirement 'native' cannot be represented in C}}
  func nativePointer(_ item: UnsafeMutablePointer<(any SwiftItem)?>?)
  // expected-error@-1 {{type 'UnsafeMutablePointer<(any SwiftItem)?>?' of COM interface requirement 'nativePointer' cannot be represented in C}}
  func value(_ item: SwiftValue)
  // expected-error@-1 {{type 'SwiftValue' of COM interface requirement 'value' cannot be represented in C}}
  func nestedOptional(_ item: Item??)
  // expected-error@-1 {{type 'Item??' (aka 'Optional<Optional<any IItem>>') of COM interface requirement 'nestedOptional' cannot be represented in C}}
  func nestedResult() -> Item??
  // expected-error@-1 {{type 'Item??' (aka 'Optional<Optional<any IItem>>') of COM interface requirement 'nestedResult()' cannot be represented in C}}
  func nestedPointer(_ item: UnsafeMutablePointer<Item??>)
  // expected-error@-1 {{type 'UnsafeMutablePointer<Item??>' (aka 'UnsafeMutablePointer<Optional<Optional<any IItem>>>') of COM interface requirement 'nestedPointer' cannot be represented in C}}
  func metatype(_ item: Item.Type)
  // expected-error@-1 {{type '(any IItem).Type' of COM interface requirement 'metatype' cannot be represented in C}}
  func array(_ items: [Item])
  // expected-error@-1 {{type '[Item]' (aka 'Array<any IItem>') of COM interface requirement 'array' cannot be represented in C}}
}
