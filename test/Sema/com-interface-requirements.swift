// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t -D COM_INTEROP -verify-additional-prefix com-
// RUN: %target-typecheck-verify-swift

#if COM_INTEROP
// Ordinary C-compatible methods and accessors remain valid. The imported
// COMInterface identity requirement returns a GUID struct and is exempt from
// these interface-vtable checks.
@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IValid {
  typealias Integer = Int32
  func empty()
  func explicitVoid() -> Void
  func scalar(_ value: Int32) -> UInt32
  func floating(_ value: Float) -> Double
  func pointer(_ value: UnsafeRawPointer?) -> UnsafeMutableRawPointer?
  func outParameter(_ value: UnsafeMutablePointer<Int32>?)
  var readOnly: Int32 { get }
  var readWrite: Int32 { get set }
  subscript(index: Int32) -> Int32 { get set }
}

@com(interface: "10000000-0000-0000-0000-000000000002")
protocol IRefined: IValid {
  func another(_ value: UInt8)
}

// Extension members do not introduce COM vtable requirements.
extension IValid {
  static func helper<T>(_ value: T) -> T { value }
  func asynchronousHelper() async throws {}
  func swiftResult() -> [Int] { [] }
}

struct SwiftValue {}

@com(interface: "20000000-0000-0000-0000-000000000001")
protocol IAssociated {
  associatedtype Element // expected-com-error {{requirement 'Element' cannot be used in a COM interface}}
}

// Initializers must be rejected without attempting to treat them as FuncDecls.
@com(interface: "20000000-0000-0000-0000-000000000002")
protocol IInitializer {
  init() // expected-com-error {{requirement 'init()' cannot be used in a COM interface}}
}

@com(interface: "20000000-0000-0000-0000-000000000003")
protocol IStatic {
  static func create() // expected-com-error {{requirement 'create()' cannot be static in a COM interface}}
}

@com(interface: "20000000-0000-0000-0000-000000000004")
protocol IGeneric {
  func generic<T>(_ value: T) // expected-com-error {{requirement 'generic' cannot be generic in a COM interface}}
  // expected-com-error@-1 {{type 'T' of COM interface requirement 'generic' cannot be represented in C}}
}

@com(interface: "20000000-0000-0000-0000-000000000005")
protocol IEffects {
  func asynchronous() async // expected-com-error {{requirement 'asynchronous()' cannot be async in a COM interface}}
  func throwing() throws // expected-com-error {{requirement 'throwing()' cannot be throwing in a COM interface}}
  func both() async throws // expected-com-error {{requirement 'both()' cannot be async in a COM interface}}
  // expected-com-error@-1 {{requirement 'both()' cannot be throwing in a COM interface}}
}

@com(interface: "20000000-0000-0000-0000-000000000006")
protocol ITypes {
  func result() -> SwiftValue // expected-com-error {{type 'SwiftValue' of COM interface requirement 'result()' cannot be represented in C}}
  func parameter(_ value: SwiftValue) // expected-com-error {{type 'SwiftValue' of COM interface requirement 'parameter' cannot be represented in C}}
  func several(_ first: SwiftValue, _ second: SwiftValue) -> SwiftValue // expected-com-error 3 {{type 'SwiftValue' of COM interface requirement 'several' cannot be represented in C}}
}

@com(interface: "20000000-0000-0000-0000-000000000007")
protocol IParameters {
  func variadic(_ values: Int32...) // expected-com-error {{parameter 'values' of COM interface requirement 'variadic' cannot be represented in C}}
  // expected-com-error@-1 {{type 'Int32...' of COM interface requirement 'variadic' cannot be represented in C}}
  func modify(_ value: inout Int32) // expected-com-error {{parameter 'value' of COM interface requirement 'modify' cannot be represented in C}}
  func borrow(_ value: borrowing Int32) // expected-com-error {{parameter 'value' of COM interface requirement 'borrow' cannot be represented in C}}
  func consume(_ value: consuming Int32) // expected-com-error {{parameter 'value' of COM interface requirement 'consume' cannot be represented in C}}
  func legacyBorrow(_ value: __shared Int32) // expected-com-error {{parameter 'value' of COM interface requirement 'legacyBorrow' cannot be represented in C}}
  func legacyConsume(_ value: __owned Int32) // expected-com-error {{parameter 'value' of COM interface requirement 'legacyConsume' cannot be represented in C}}
}

@com(interface: "20000000-0000-0000-0000-000000000008")
protocol IProperties {
  static var staticProperty: Int32 { get } // expected-com-error {{requirement 'staticProperty' cannot be static in a COM interface}}
  var effectful: Int32 { get async throws } // expected-com-error {{requirement 'effectful' cannot be async in a COM interface}}
  // expected-com-error@-1 {{requirement 'effectful' cannot be throwing in a COM interface}}
  var unsupported: SwiftValue { get set } // expected-com-error 2 {{type 'SwiftValue' of COM interface requirement 'unsupported' cannot be represented in C}}
  // The index is also checked in the synthesized modify accessor.
  subscript(value: SwiftValue) -> SwiftValue { get set } // expected-com-error 5 {{type 'SwiftValue' of COM interface requirement 'subscript(_:)' cannot be represented in C}}
  subscript<T>(generic value: T) -> Int32 { get } // expected-com-error {{requirement 'subscript(generic:)' cannot be generic in a COM interface}}
  // expected-com-error@-1 {{type 'T' of COM interface requirement 'subscript(generic:)' cannot be represented in C}}
}
#endif

// The COM-specific restrictions must not affect ordinary Swift protocols,
// whether COM interop is enabled or disabled.
protocol Ordinary {
  associatedtype Element
  init()
  static func factory()
  func generic<T>(_ value: T) -> T
  func asynchronous() async
  func throwing() throws
  func array(_ values: [Int]) -> [Int]
  func variadic(_ values: Int...)
  func modify(_ value: inout Int)
  func borrow(_ value: borrowing Int)
  func consume(_ value: consuming Int)
  var effectful: Int { get async throws }
}
