// RUN: %swift %s -parse -verify

@public protocol PublicProto {
  func publicReq()
}

// expected-note@+1 + {{type declared here}}
@internal protocol InternalProto {
  func internalReq()
}

// expected-note@+1 + {{type declared here}}
@private protocol PrivateProto {
  func privateReq()
}

@public struct PublicStruct: PublicProto, InternalProto, PrivateProto {
  @private func publicReq() {} // expected-error {{method 'publicReq()' must have public accessibility because it matches a requirement in public protocol 'PublicProto'}} {{3-11=@public}}
  @private func internalReq() {} // expected-error {{method 'internalReq()' must have internal accessibility because it matches a requirement in internal protocol 'InternalProto'}} {{3-11=@internal}}
  @private func privateReq() {}
}

// expected-note@+1 + {{type declared here}}
@internal struct InternalStruct: PublicProto, InternalProto, PrivateProto {
  @private func publicReq() {} // expected-error {{method 'publicReq()' must be as accessible as its enclosing type because it matches a requirement in protocol 'PublicProto'}} {{3-11=@internal}}
  @private func internalReq() {} // expected-error {{method 'internalReq()' must have internal accessibility because it matches a requirement in internal protocol 'InternalProto'}} {{3-11=@internal}}
  @private func privateReq() {}
}

// expected-note@+1 + {{type declared here}}
@private struct PrivateStruct: PublicProto, InternalProto, PrivateProto {
  @private func publicReq() {}
  @private func internalReq() {}
  @private func privateReq() {}
}

@public struct PublicStructDefaultMethods: PublicProto, InternalProto, PrivateProto {
  func publicReq() {} // expected-error {{method 'publicReq()' must have public accessibility because it matches a requirement in public protocol 'PublicProto'}} {{3-3=@public }}
  func internalReq() {}
  func privateReq() {}
}


@public class Base {
  @public @required init() {}
}

@public class PublicSub: Base {
  init() {} // expected-error {{'required' initializer must be as accessible as its enclosing type}} {{3-3=@public }}
}

@internal class InternalSub: Base {
  @private init() {} // expected-error {{'required' initializer must be as accessible as its enclosing type}} {{3-11=@internal}}
}

@internal class InternalSubGood: Base {
  init() {} // no-warning
}


@public typealias PublicTA1 = PublicStruct
@public typealias PublicTA2 = InternalStruct // expected-error {{type alias cannot be declared public because its underlying type uses an internal type}}
@public typealias PublicTA3 = PrivateStruct // expected-error {{type alias cannot be declared public because its underlying type uses a private type}}

// expected-note@+1 {{type declared here}}
@internal typealias InternalTA1 = PublicStruct
@internal typealias InternalTA2 = InternalStruct
@internal typealias InternalTA3 = PrivateStruct // expected-error {{type alias cannot be declared internal because its underlying type uses a private type}}

@public typealias PublicFromInternal = InternalTA1 // expected-error {{type alias cannot be declared public because its underlying type uses an internal type}}

typealias FunctionType1 = PrivateStruct -> PublicStruct // expected-error {{type alias must be declared private because its underlying type uses a private type}}
typealias FunctionType2 = PublicStruct -> PrivateStruct // expected-error {{type alias must be declared private because its underlying type uses a private type}}
typealias FunctionType3 = PrivateStruct -> PrivateStruct // expected-error {{type alias must be declared private because its underlying type uses a private type}}

typealias ArrayType = [PrivateStruct] // expected-error {{type alias must be declared private because its underlying type uses a private type}}
typealias DictType = [String : PrivateStruct] // expected-error {{type alias must be declared private because its underlying type uses a private type}}
typealias GenericArgs = Optional<PrivateStruct> // expected-error {{type alias must be declared private because its underlying type uses a private type}}


@public protocol HasAssocType {
  typealias Inferred
  func test(input: Inferred)
}

@public struct AssocTypeImpl: HasAssocType {
  @public func test(input: Bool) {}
}
@public let _: AssocTypeImpl.Inferred?


@public let x: PrivateStruct = PrivateStruct() // expected-error {{constant cannot be declared public because its type uses a private type}}
@public var a: PrivateStruct?, b: PrivateStruct? // expected-error 2 {{variable cannot be declared public because its type uses a private type}}
@public var (c, d): (PrivateStruct?, PrivateStruct?) // expected-error {{variable cannot be declared public because its type uses a private type}}

var internalVar: PrivateStruct? // expected-error {{variable must be declared private because its type uses a private type}}

let internalConstant = PrivateStruct() // expected-error {{constant must be declared private because its type 'PrivateStruct' uses a private type}}
@public let publicConstant = [InternalStruct]() // expected-error {{constant cannot be declared public because its type '[(InternalStruct)]' uses an internal type}}

@public struct Properties {
  @public let x: PrivateStruct = PrivateStruct() // expected-error {{property cannot be declared public because its type uses a private type}}
  @public var a: PrivateStruct?, b: PrivateStruct? // expected-error 2 {{property cannot be declared public because its type uses a private type}}
  @public var (c, d): (PrivateStruct?, PrivateStruct?) // expected-error {{property cannot be declared public because its type uses a private type}}

  let y = PrivateStruct() // expected-error {{property must be declared private because its type 'PrivateStruct' uses a private type}}
}

struct Subscripts {
  subscript (a: PrivateStruct) -> Int { return 0 } // expected-error {{subscript must be declared private because its index uses a private type}}
  subscript (a: Int) -> PrivateStruct { return PrivateStruct() } // expected-error {{subscript must be declared private because its element type uses a private type}}

  @public subscript (a: PrivateStruct, b: Int) -> Int { return 0 } // expected-error {{subscript cannot be declared public because its index uses a private type}}
  @public subscript (a: Int, b: PrivateStruct) -> Int { return 0 } // expected-error {{subscript cannot be declared public because its index uses a private type}}
  @public subscript (a: InternalStruct, b: PrivateStruct) -> InternalStruct { return InternalStruct() } // expected-error {{subscript cannot be declared public because its index uses a private type}}
  @public subscript (a: PrivateStruct, b: InternalStruct) -> PrivateStruct { return PrivateStruct() } // expected-error {{subscript cannot be declared public because its index uses a private type}}
  @public subscript (a: Int, b: Int) -> InternalStruct { return InternalStruct() } // expected-error {{subscript cannot be declared public because its element type uses an internal type}}
}

struct Methods {
  func foo(a: PrivateStruct) -> Int { return 0 } // expected-error {{method must be declared private because its parameter uses a private type}}
  func bar(a: Int) -> PrivateStruct { return PrivateStruct() } // expected-error {{method must be declared private because its result uses a private type}}

  @public func a(a: PrivateStruct, b: Int) -> Int { return 0 } // expected-error {{method cannot be declared public because its parameter uses a private type}}
  @public func b(a: Int, b: PrivateStruct) -> Int { return 0 } // expected-error {{method cannot be declared public because its parameter uses a private type}}
  @public func c(a: InternalStruct, b: PrivateStruct) -> InternalStruct { return InternalStruct() } // expected-error {{method cannot be declared public because its parameter uses a private type}}
  @public func d(a: PrivateStruct, b: InternalStruct) -> PrivateStruct { return PrivateStruct() } // expected-error {{method cannot be declared public because its parameter uses a private type}}
  @public func e(a: Int, b: Int) -> InternalStruct { return InternalStruct() } // expected-error {{method cannot be declared public because its result uses an internal type}}
}
func privateParam(a: PrivateStruct) {} // expected-error {{function must be declared private because its parameter uses a private type}}

struct Initializers {
  init(a: PrivateStruct) {} // expected-error {{initializer must be declared private because its parameter uses a private type}}

  @public init(a: PrivateStruct, b: Int) {} // expected-error {{initializer cannot be declared public because its parameter uses a private type}}
  @public init(a: Int, b: PrivateStruct) {} // expected-error {{initializer cannot be declared public because its parameter uses a private type}}
  @public init(a: InternalStruct, b: PrivateStruct) {} // expected-error {{initializer cannot be declared public because its parameter uses a private type}}
  @public init(a: PrivateStruct, b: InternalStruct) { } // expected-error {{initializer cannot be declared public because its parameter uses a private type}}
}


@public class PublicClass {}
// expected-note@+1 + {{type declared here}}
@internal class InternalClass {}
// expected-note@+1 + {{type declared here}}
@private class PrivateClass {}

@public protocol AssocTypes {
  typealias Foo

  typealias Internal: InternalClass // expected-error {{associated type in a public protocol uses an internal type in its requirement}}
  typealias InternalConformer: InternalProto // expected-error {{associated type in a public protocol uses an internal type in its requirement}}
  typealias PrivateConformer: PrivateProto // expected-error {{associated type in a public protocol uses a private type in its requirement}}
  typealias PI: PrivateProto, InternalProto // expected-error {{associated type in a public protocol uses a private type in its requirement}}
  typealias IP: InternalProto, PrivateProto // expected-error {{associated type in a public protocol uses a private type in its requirement}}

  typealias PrivateDefault = PrivateStruct // expected-error {{associated type in a public protocol uses a private type in its default definition}}
  typealias PublicDefault = PublicStruct
  typealias PrivateDefaultConformer: PublicProto = PrivateStruct // expected-error {{associated type in a public protocol uses a private type in its default definition}}
  typealias PublicDefaultConformer: PrivateProto = PublicStruct // expected-error {{associated type in a public protocol uses a private type in its requirement}}
  typealias PrivatePrivateDefaultConformer: PrivateProto = PrivateStruct // expected-error {{associated type in a public protocol uses a private type in its requirement}}
  typealias PublicPublicDefaultConformer: PublicProto = PublicStruct
}

@public protocol RequirementTypes {
  var x: PrivateStruct { get } // expected-error {{property cannot be declared public because its type uses a private type}}
  subscript(x: Int) -> InternalStruct { get set } // expected-error {{subscript cannot be declared public because its element type uses an internal type}}
  func foo() -> PrivateStruct // expected-error {{method cannot be declared public because its result uses a private type}}
  init(x: PrivateStruct) // expected-error {{initializer cannot be declared public because its parameter uses a private type}}
}

protocol DefaultRefinesPrivate : PrivateProto {} // expected-error {{protocol must be declared private because it refines a private protocol}}
@public protocol PublicRefinesPrivate : PrivateProto {} // expected-error {{public protocol cannot refine a private protocol}}
@public protocol PublicRefinesInternal : InternalProto {} // expected-error {{public protocol cannot refine an internal protocol}}
@public protocol PublicRefinesPI : PrivateProto, InternalProto {} // expected-error {{public protocol cannot refine a private protocol}}
@public protocol PublicRefinesIP : InternalProto, PrivateProto {} // expected-error {{public protocol cannot refine a private protocol}}


// expected-note@+1 + {{type declared here}}
@private typealias PrivateInt = Int
enum DefaultRawPrivate : PrivateInt { // expected-error {{enum must be declared private because its raw type uses a private type}}
  case A
}
@public enum PublicRawPrivate : PrivateInt { // expected-error {{enum cannot be declared public because its raw type uses a private type}}
  case A
}
@public enum MultipleConformance : PrivateProto, PrivateInt { // expected-error {{enum cannot be declared public because its raw type uses a private type}} expected-error {{must appear first}}
  case A
  func privateReq() {}
}

@public class PublicSubclassPublic : PublicClass {}
@public class PublicSubclassInternal : InternalClass {} // expected-error {{class cannot be declared public because its superclass is internal}}
@public class PublicSubclassPrivate : PrivateClass {} // expected-error {{class cannot be declared public because its superclass is private}}

class DefaultSubclassPublic : PublicClass {}
class DefaultSubclassInternal : InternalClass {}
class DefaultSubclassPrivate : PrivateClass {} // expected-error {{class must be declared private because its superclass is private}}


@public enum PublicEnumPrivate {
  case A(PrivateStruct) // expected-error {{enum case in a public enum uses a private type}}
}
enum DefaultEnumPrivate {
  case A(PrivateStruct) // expected-error {{enum case in an internal enum uses a private type}}
}
@public enum PublicEnumPI {
  case A(InternalStruct) // expected-error {{enum case in a public enum uses an internal type}}
  case B(PrivateStruct, InternalStruct) // expected-error {{enum case in a public enum uses a private type}}
  case C(InternalStruct, PrivateStruct) // expected-error {{enum case in a public enum uses a private type}}
}
enum DefaultEnumPublic {
  case A(PublicStruct) // no-warning
}
