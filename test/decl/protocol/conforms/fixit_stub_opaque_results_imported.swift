// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/Factories.swift -module-name Factories -emit-module-path %t/Factories.swiftmodule
// RUN: %target-swift-frontend -typecheck -verify -I %t %t/Client.swift

//--- Factories.swift
public protocol Widget {}
public protocol Factory {
  associatedtype Product: Widget
  func make() -> Product
}
public protocol Response {}
public protocol Promise<Value> {
  associatedtype Value
}
public protocol Fetcher {
  associatedtype Output: Promise<any Response>
  func fetch(url: String) throws -> Output
}

//--- Client.swift
import Factories

struct LibraryFactory: Factory {} // expected-error {{does not conform}}
// expected-note@-1 {{add stubs for conformance}}
// expected-note@-2 {{add stubs using opaque result types}}{{33-33=\n    func make() -> some Factories.Widget {\n        <#code#>\n    \}\n}}
// expected-note@Factories.Factory.Product:2 {{protocol requires nested type 'Product'}}

struct LibraryFetcher: Fetcher {} // expected-error {{does not conform}}
// expected-note@-1 {{add stubs for conformance}}
// expected-note@-2 {{add stubs using opaque result types}}{{33-33=\n    func fetch(url: String) throws -> some Factories.Promise<any Factories.Response> {\n        <#code#>\n    \}\n}}
// expected-note@Factories.Fetcher.Output:2 {{protocol requires nested type 'Output'}}
