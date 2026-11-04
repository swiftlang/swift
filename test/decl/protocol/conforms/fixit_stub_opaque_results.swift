// RUN: %target-typecheck-verify-swift

public protocol Widget {}
struct Button: Widget {}

protocol Factory {
  associatedtype Product: Widget // expected-note {{protocol requires nested type 'Product'}}
  func make() -> Product
}

struct MyFactory: Factory {} // expected-error {{type 'MyFactory' does not conform to protocol 'Factory'}}
// expected-note@-1 {{add stubs for conformance}}{{28-28=\n    typealias Product = <#type#>\n}}
// expected-note@-2 {{add stubs using opaque result types}}{{28-28=\n    func make() -> some Widget {\n        <#code#>\n    \}\n}}

protocol Response {}
protocol Promise<Value> {
  associatedtype Value
}
protocol Fetcher {
  associatedtype Output: Promise<any Response> // expected-note {{protocol requires nested type 'Output'}}
  func fetch(url: String) async throws -> Output
}

struct MyFetcher: Fetcher {} // expected-error {{type 'MyFetcher' does not conform to protocol 'Fetcher'}}
// expected-note@-1 {{add stubs for conformance}}{{28-28=\n    typealias Output = <#type#>\n}}
// expected-note@-2 {{add stubs using opaque result types}}{{28-28=\n    func fetch(url: String) async throws -> some Promise<any Response> {\n        <#code#>\n    \}\n}}

public protocol PublicFactory {
  associatedtype Product: Widget // expected-note {{protocol requires nested type 'Product'}}
  static func make(_ count: Int) throws -> Product
}
public struct ExtensionFactory {}
extension ExtensionFactory: PublicFactory {} // expected-error {{does not conform}}
// expected-note@-1 {{add stubs for conformance}}
// expected-note@-2 {{add stubs using opaque result types}}{{44-44=\n    public static func make(_ count: Int) throws -> some Widget {\n        <#code#>\n    \}\n}}

// Two methods must return the same type. Independent opaque results would not
// satisfy this requirement.
protocol SharedResult {
  associatedtype Product: Widget // expected-note {{protocol requires nested type 'Product'}}
  func first() -> Product
  func second() -> Product
}
struct Shared: SharedResult {} // expected-error {{does not conform}}
// expected-note@-1 {{add stubs for conformance}}

protocol InputResult {
  associatedtype Product: Widget // expected-note {{protocol requires nested type 'Product'}}
  func make(from: Product) -> Product
}
struct Input: InputResult {} // expected-error {{does not conform}}
// expected-note@-1 {{add stubs for conformance}}

protocol PropertyResult {
  associatedtype Product: Widget // expected-note {{protocol requires nested type 'Product'}}
  var product: Product { get }
}
struct Property: PropertyResult {} // expected-error {{does not conform}}
// expected-note@-1 {{add stubs for conformance}}

protocol WrappedResult {
  associatedtype Product: Widget // expected-note {{protocol requires nested type 'Product'}}
  func make() -> Product?
}
struct Wrapped: WrappedResult {} // expected-error {{does not conform}}
// expected-note@-1 {{add stubs for conformance}}

protocol GenericResult {
  associatedtype Product: Widget // expected-note {{protocol requires nested type 'Product'}}
  func make<T>(from: T) -> Product
}
struct Generic: GenericResult {} // expected-error {{does not conform}}
// expected-note@-1 {{add stubs for conformance}}

protocol UnconstrainedResult {
  associatedtype Product // expected-note {{protocol requires nested type 'Product'}}
  func make() -> Product
}
struct Unconstrained: UnconstrainedResult {} // expected-error {{does not conform}}
// expected-note@-1 {{add stubs for conformance}}

protocol DefaultResult {
  associatedtype Product: Widget = Button
  func make() -> Product // expected-note 2 {{protocol requires function 'make()' with type}}
}
struct Default: DefaultResult {} // expected-error {{does not conform}}
// expected-note@-1 {{add stubs for conformance}}
struct Explicit: DefaultResult { // expected-error {{does not conform}}
  // expected-note@-1 {{add stubs for conformance}}
  typealias Product = Button
}

protocol ConcreteResult {
  func make() -> Button // expected-note {{protocol requires function 'make()' with type '() -> Button'}}
}
struct Concrete: ConcreteResult {} // expected-error {{does not conform}}
// expected-note@-1 {{add stubs for conformance}}

protocol InheritedFactory: Factory {}
struct Inherited: InheritedFactory {} // expected-error {{does not conform to protocol 'Factory'}}
// expected-note@-1 {{add stubs for conformance}}
// expected-note@7 {{protocol requires nested type 'Product'}}

protocol RelatedResults {
  associatedtype Product: Widget // expected-note {{protocol requires nested type 'Product'}}
  associatedtype Other: Widget where Other == Product // expected-note {{protocol requires nested type 'Other'}}
  func make() -> Product
}
struct Related: RelatedResults {} // expected-error {{does not conform}}
// expected-note@-1 {{add stubs for conformance}}

protocol NonPrimary {
  associatedtype Value
}
protocol NonPrimaryResult {
  associatedtype Product: NonPrimary where Product.Value == Int // expected-note {{protocol requires nested type 'Product'}}
  func make() -> Product
}
struct NestedConstraint: NonPrimaryResult {} // expected-error {{does not conform}}
// expected-note@-1 {{add stubs for conformance}}

protocol Pair<First, Second> {
  associatedtype First
  associatedtype Second
}
protocol PartialPrimaryResult {
  associatedtype Product: Pair where Product.First == Int // expected-note {{protocol requires nested type 'Product'}}
  func make() -> Product
}
struct PartialPrimary: PartialPrimaryResult {} // expected-error {{does not conform}}
// expected-note@-1 {{add stubs for conformance}}

protocol DefaultMethodResult {
  associatedtype Product: Widget // expected-note {{protocol requires nested type 'Product'}}
  func make() -> Product
}
extension DefaultMethodResult {
  func make() -> Product { fatalError() }
}
struct DefaultMethod: DefaultMethodResult {} // expected-error {{does not conform}}
// expected-note@-1 {{add stubs for conformance}}

protocol InputElsewhereResult {
  associatedtype Product: Widget // expected-note {{protocol requires nested type 'Product'}}
  func make() -> Product
  func consume(_: Product)
}
struct InputElsewhere: InputElsewhereResult {} // expected-error {{does not conform}}
// expected-note@-1 {{add stubs for conformance}}

protocol ThrowingResult {
  associatedtype Product: Error // expected-note {{protocol requires nested type 'Product'}}
  func make() throws(Product) -> Product
}
struct TypedThrows: ThrowingResult {} // expected-error {{does not conform}}
// expected-note@-1 {{add stubs for conformance}}

class BaseWidget {}
protocol SuperclassResult {
  associatedtype Product: BaseWidget, Widget // expected-note {{protocol requires nested type 'Product'}}
  func make() -> Product
}
struct Superclass: SuperclassResult {} // expected-error {{does not conform}}
// expected-note@-1 {{add stubs for conformance}}

protocol ExplicitResult {
  associatedtype Product: Widget
  func make() -> Product // expected-note {{protocol requires function 'make()' with type}}
}
struct ExplicitWitness: ExplicitResult { // expected-error {{does not conform}}
  // expected-note@-1 {{add stubs for conformance}}
  typealias Product = Button
}

protocol ExistingMethodResult {
  associatedtype Product: Widget // expected-note {{unable to infer associated type 'Product' for protocol 'ExistingMethodResult'}}
  func make() -> Product
}
struct ExistingMethod: ExistingMethodResult { // expected-error {{does not conform}}
  func make() -> Int { 0 } // expected-note {{candidate would match and infer 'Product' = 'Int' if 'Int' conformed to 'Widget'}}
}

protocol OverloadedResults {
  associatedtype First: Widget // expected-note {{protocol requires nested type 'First'}}
  associatedtype Second: Widget // expected-note {{protocol requires nested type 'Second'}}
  func make() -> First
  func make() -> Second
}
struct Overloaded: OverloadedResults {} // expected-error {{does not conform}}
// expected-note@-1 {{add stubs for conformance}}
