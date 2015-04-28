// RUN: %target-parse-verify-swift

public protocol PublicProto {
  func publicReq()
}

// expected-note@+1 + {{type declared here}}
internal protocol InternalProto {
  func internalReq()
}

// expected-note@+1 + {{type declared here}}
private protocol PrivateProto {
  func privateReq()
}

public struct PublicStruct: PublicProto, InternalProto, PrivateProto {
  private func publicReq() {} // expected-error {{method 'publicReq()' must be declared public because it matches a requirement in public protocol 'PublicProto'}} {{3-10=public}}
  private func internalReq() {} // expected-error {{method 'internalReq()' must be declared internal because it matches a requirement in internal protocol 'InternalProto'}} {{3-10=internal}}
  private func privateReq() {}

  public var publicVar = 0
}

// expected-note@+1 + {{type declared here}}
internal struct InternalStruct: PublicProto, InternalProto, PrivateProto {
  private func publicReq() {} // expected-error {{method 'publicReq()' must be as accessible as its enclosing type because it matches a requirement in protocol 'PublicProto'}} {{3-10=internal}}
  private func internalReq() {} // expected-error {{method 'internalReq()' must be declared internal because it matches a requirement in internal protocol 'InternalProto'}} {{3-10=internal}}
  private func privateReq() {}

  public var publicVar = 0 // expected-warning {{declaring a public var for an internal struct}} {{3-9=internal}}
}

// expected-note@+1 + {{type declared here}}
private struct PrivateStruct: PublicProto, InternalProto, PrivateProto {
  private func publicReq() {}
  private func internalReq() {}
  private func privateReq() {}

  public var publicVar = 0 // expected-warning {{declaring a public var for a private struct}} {{3-9=private}}
}

extension PublicStruct {
  public init(x: Int) { self.init() }
}

extension InternalStruct {
  public init(x: Int) { self.init() } // expected-warning {{declaring a public initializer for an internal struct}} {{3-9=internal}}
}

extension PrivateStruct {
  public init(x: Int) { self.init() } // expected-warning {{declaring a public initializer for a private struct}} {{3-9=private}}
}

public extension PublicStruct {
  public func extMemberPublic() {}
  private func extImplPublic() {}
}
internal extension PublicStruct {
  public func extMemberInternal() {} // expected-warning {{declaring a public instance method in an internal extension}}
  private func extImplInternal() {}
}
private extension PublicStruct {
  public func extMemberPrivate() {} // expected-warning {{declaring a public instance method in a private extension}}
  private func extImplPrivate() {}
}
public extension InternalStruct { // expected-error {{extension of internal struct cannot be declared public}} {{1-7=}}
  public func extMemberPublic() {} // expected-warning {{declaring a public instance method for an internal struct}} {{3-9=internal}}
  private func extImplPublic() {}
}
internal extension InternalStruct {
  public func extMemberInternal() {} // expected-warning {{declaring a public instance method in an internal extension}}
  private func extImplInternal() {}
}
private extension InternalStruct {
  public func extMemberPrivate() {} // expected-warning {{declaring a public instance method in a private extension}}
  private func extImplPrivate() {}
}
public extension PrivateStruct { // expected-error {{extension of private struct cannot be declared public}} {{1-7=}}
  public func extMemberPublic() {} // expected-warning {{declaring a public instance method for a private struct}} {{3-9=private}}
  private func extImplPublic() {}
}
internal extension PrivateStruct { // expected-error {{extension of private struct cannot be declared internal}} {{1-9=}}
  public func extMemberInternal() {} // expected-warning {{declaring a public instance method in an internal extension}}
  private func extImplInternal() {}
}
private extension PrivateStruct {
  public func extMemberPrivate() {} // expected-warning {{declaring a public instance method in a private extension}}
  private func extImplPrivate() {}
}


public struct PublicStructDefaultMethods: PublicProto, InternalProto, PrivateProto {
  func publicReq() {} // expected-error {{method 'publicReq()' must be declared public because it matches a requirement in public protocol 'PublicProto'}} {{3-3=public }}
  func internalReq() {}
  func privateReq() {}
}


public class Base {
  required public init() {}
  // expected-note@+1 + {{overridden declaration is here}}
  public func foo() {}
  // expected-note@+1 + {{overridden declaration is here}}
  public internal(set) var bar: Int = 0
  // expected-note@+1 + {{overridden declaration is here}}
  public subscript () -> () { return () }
}

public class PublicSub: Base {
  required init() {} // expected-error {{'required' initializer must be as accessible as its enclosing type}} {{12-12=public }}
  override func foo() {} // expected-error {{overriding instance method must be as accessible as the declaration it overrides}} {{12-12=public }}
  override var bar: Int { // expected-error {{overriding var must be as accessible as the declaration it overrides}} {{12-12=public }}
    get { return 0 }
    set {}
  }
  override subscript () -> () { return () } // expected-error {{overriding subscript must be as accessible as the declaration it overrides}} {{12-12=public }}
}

internal class InternalSub: Base {
  required private init() {} // expected-error {{'required' initializer must be as accessible as its enclosing type}} {{12-19=internal}}
  private override func foo() {} // expected-error {{overriding instance method must be as accessible as its enclosing type}} {{3-10=internal}}
  private override var bar: Int { // expected-error {{overriding var must be as accessible as its enclosing type}} {{3-10=internal}}
    get { return 0 }
    set {}
  }
  private override subscript () -> () { return () } // expected-error {{overriding subscript must be as accessible as its enclosing type}} {{3-10=internal}}
}

internal class InternalSubGood: Base {
  required init() {} // no-warning
  override func foo() {}
  override var bar: Int {
    get { return 0 }
    set {}
  }
  override subscript () -> () { return () }
}

internal class InternalSubPrivateSet: Base {
  required init() {}
  private(set) override var bar: Int { // expected-error {{overriding var must be as accessible as its enclosing type}} {{3-16=}}
    get { return 0 }
    set {}
  }
  private(set) override subscript () -> () { // okay; read-only in base class
    get { return () }
    set {}
  }
}


public typealias PublicTA1 = PublicStruct
public typealias PublicTA2 = InternalStruct // expected-error {{type alias cannot be declared public because its underlying type uses an internal type}}
public typealias PublicTA3 = PrivateStruct // expected-error {{type alias cannot be declared public because its underlying type uses a private type}}

// expected-note@+1 {{type declared here}}
internal typealias InternalTA1 = PublicStruct
internal typealias InternalTA2 = InternalStruct
internal typealias InternalTA3 = PrivateStruct // expected-error {{type alias cannot be declared internal because its underlying type uses a private type}}

public typealias PublicFromInternal = InternalTA1 // expected-error {{type alias cannot be declared public because its underlying type uses an internal type}}

typealias FunctionType1 = PrivateStruct -> PublicStruct // expected-error {{type alias must be declared private because its underlying type uses a private type}}
typealias FunctionType2 = PublicStruct -> PrivateStruct // expected-error {{type alias must be declared private because its underlying type uses a private type}}
typealias FunctionType3 = PrivateStruct -> PrivateStruct // expected-error {{type alias must be declared private because its underlying type uses a private type}}

typealias ArrayType = [PrivateStruct] // expected-error {{type alias must be declared private because its underlying type uses a private type}}
typealias DictType = [String : PrivateStruct] // expected-error {{type alias must be declared private because its underlying type uses a private type}}
typealias GenericArgs = Optional<PrivateStruct> // expected-error {{type alias must be declared private because its underlying type uses a private type}}


public protocol HasAssocType {
  typealias Inferred
  func test(input: Inferred)
}

public struct AssocTypeImpl: HasAssocType {
  public func test(input: Bool) {}
}
public let _: AssocTypeImpl.Inferred?


public let x: PrivateStruct = PrivateStruct() // expected-error {{constant cannot be declared public because its type uses a private type}}
public var a: PrivateStruct?, b: PrivateStruct? // expected-error 2 {{variable cannot be declared public because its type uses a private type}}
public var (c, d): (PrivateStruct?, PrivateStruct?) // expected-error {{variable cannot be declared public because its type uses a private type}}

var internalVar: PrivateStruct? // expected-error {{variable must be declared private because its type uses a private type}}

let internalConstant = PrivateStruct() // expected-error {{constant must be declared private because its type 'PrivateStruct' uses a private type}}
public let publicConstant = [InternalStruct]() // expected-error {{constant cannot be declared public because its type '[InternalStruct]' uses an internal type}}

public struct Properties {
  public let x: PrivateStruct = PrivateStruct() // expected-error {{property cannot be declared public because its type uses a private type}}
  public var a: PrivateStruct?, b: PrivateStruct? // expected-error 2 {{property cannot be declared public because its type uses a private type}}
  public var (c, d): (PrivateStruct?, PrivateStruct?) // expected-error {{property cannot be declared public because its type uses a private type}}

  let y = PrivateStruct() // expected-error {{property must be declared private because its type 'PrivateStruct' uses a private type}}
}

public struct Subscripts {
  subscript (a: PrivateStruct) -> Int { return 0 } // expected-error {{subscript must be declared private because its index uses a private type}}
  subscript (a: Int) -> PrivateStruct { return PrivateStruct() } // expected-error {{subscript must be declared private because its element type uses a private type}}

  public subscript (a: PrivateStruct, b: Int) -> Int { return 0 } // expected-error {{subscript cannot be declared public because its index uses a private type}}
  public subscript (a: Int, b: PrivateStruct) -> Int { return 0 } // expected-error {{subscript cannot be declared public because its index uses a private type}}
  public subscript (a: InternalStruct, b: PrivateStruct) -> InternalStruct { return InternalStruct() } // expected-error {{subscript cannot be declared public because its index uses a private type}}
  public subscript (a: PrivateStruct, b: InternalStruct) -> PrivateStruct { return PrivateStruct() } // expected-error {{subscript cannot be declared public because its index uses a private type}}
  public subscript (a: Int, b: Int) -> InternalStruct { return InternalStruct() } // expected-error {{subscript cannot be declared public because its element type uses an internal type}}
}

public struct Methods {
  func foo(a: PrivateStruct) -> Int { return 0 } // expected-error {{method must be declared private because its parameter uses a private type}}
  func bar(a: Int) -> PrivateStruct { return PrivateStruct() } // expected-error {{method must be declared private because its result uses a private type}}

  public func a(a: PrivateStruct, b: Int) -> Int { return 0 } // expected-error {{method cannot be declared public because its parameter uses a private type}}
  public func b(a: Int, b: PrivateStruct) -> Int { return 0 } // expected-error {{method cannot be declared public because its parameter uses a private type}}
  public func c(a: InternalStruct, b: PrivateStruct) -> InternalStruct { return InternalStruct() } // expected-error {{method cannot be declared public because its parameter uses a private type}}
  public func d(a: PrivateStruct, b: InternalStruct) -> PrivateStruct { return PrivateStruct() } // expected-error {{method cannot be declared public because its parameter uses a private type}}
  public func e(a: Int, b: Int) -> InternalStruct { return InternalStruct() } // expected-error {{method cannot be declared public because its result uses an internal type}}
}
func privateParam(a: PrivateStruct) {} // expected-error {{function must be declared private because its parameter uses a private type}}

public struct Initializers {
  init(a: PrivateStruct) {} // expected-error {{initializer must be declared private because its parameter uses a private type}}

  public init(a: PrivateStruct, b: Int) {} // expected-error {{initializer cannot be declared public because its parameter uses a private type}}
  public init(a: Int, b: PrivateStruct) {} // expected-error {{initializer cannot be declared public because its parameter uses a private type}}
  public init(a: InternalStruct, b: PrivateStruct) {} // expected-error {{initializer cannot be declared public because its parameter uses a private type}}
  public init(a: PrivateStruct, b: InternalStruct) { } // expected-error {{initializer cannot be declared public because its parameter uses a private type}}
}


public class PublicClass {}
// expected-note@+1 + {{type declared here}}
internal class InternalClass {}
// expected-note@+1 + {{type declared here}}
private class PrivateClass {}

public protocol AssocTypes {
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

public protocol RequirementTypes {
  var x: PrivateStruct { get } // expected-error {{property cannot be declared public because its type uses a private type}}
  subscript(x: Int) -> InternalStruct { get set } // expected-error {{subscript cannot be declared public because its element type uses an internal type}}
  func foo() -> PrivateStruct // expected-error {{method cannot be declared public because its result uses a private type}}
  init(x: PrivateStruct) // expected-error {{initializer cannot be declared public because its parameter uses a private type}}
}

protocol DefaultRefinesPrivate : PrivateProto {} // expected-error {{protocol must be declared private because it refines a private protocol}}
public protocol PublicRefinesPrivate : PrivateProto {} // expected-error {{public protocol cannot refine a private protocol}}
public protocol PublicRefinesInternal : InternalProto {} // expected-error {{public protocol cannot refine an internal protocol}}
public protocol PublicRefinesPI : PrivateProto, InternalProto {} // expected-error {{public protocol cannot refine a private protocol}}
public protocol PublicRefinesIP : InternalProto, PrivateProto {} // expected-error {{public protocol cannot refine a private protocol}}


// expected-note@+1 + {{type declared here}}
private typealias PrivateInt = Int
enum DefaultRawPrivate : PrivateInt { // expected-error {{enum must be declared private because its raw type uses a private type}}
  case A
}
public enum PublicRawPrivate : PrivateInt { // expected-error {{enum cannot be declared public because its raw type uses a private type}}
  case A
}
public enum MultipleConformance : PrivateProto, PrivateInt { // expected-error {{enum cannot be declared public because its raw type uses a private type}} expected-error {{must appear first}}
  case A
  func privateReq() {}
}

public class PublicSubclassPublic : PublicClass {}
public class PublicSubclassInternal : InternalClass {} // expected-error {{class cannot be declared public because its superclass is internal}}
public class PublicSubclassPrivate : PrivateClass {} // expected-error {{class cannot be declared public because its superclass is private}}

class DefaultSubclassPublic : PublicClass {}
class DefaultSubclassInternal : InternalClass {}
class DefaultSubclassPrivate : PrivateClass {} // expected-error {{class must be declared private because its superclass is private}}


public enum PublicEnumPrivate {
  case A(PrivateStruct) // expected-error {{enum case in a public enum uses a private type}}
}
enum DefaultEnumPrivate {
  case A(PrivateStruct) // expected-error {{enum case in an internal enum uses a private type}}
}
public enum PublicEnumPI {
  case A(InternalStruct) // expected-error {{enum case in a public enum uses an internal type}}
  case B(PrivateStruct, InternalStruct) // expected-error {{enum case in a public enum uses a private type}}
  case C(InternalStruct, PrivateStruct) // expected-error {{enum case in a public enum uses a private type}}
}
enum DefaultEnumPublic {
  case A(PublicStruct) // no-warning
}

struct DefaultGeneric<T> {}
struct DefaultGenericPrivate<T: PrivateProto> {} // expected-error {{generic struct must be declared private because its generic parameter uses a private type}}
struct DefaultGenericPrivate2<T: PrivateClass> {} // expected-error {{generic struct must be declared private because its generic parameter uses a private type}}
struct DefaultGenericPrivateReq<T where T == PrivateClass> {} // expected-error {{same-type requirement makes generic parameter 'T' non-generic}}
struct DefaultGenericPrivateReq2<T where T: PrivateProto> {} // expected-error {{generic struct must be declared private because its generic requirement uses a private type}}

public struct PublicGenericInternal<T: InternalProto> {} // expected-error {{generic struct cannot be declared public because its generic parameter uses an internal type}}
public struct PublicGenericPI<T: PrivateProto, U: InternalProto> {} // expected-error {{generic struct cannot be declared public because its generic parameter uses a private type}}
public struct PublicGenericIP<T: InternalProto, U: PrivateProto> {} // expected-error {{generic struct cannot be declared public because its generic parameter uses a private type}}
public struct PublicGenericPIReq<T: PrivateProto where T: InternalProto> {} // expected-error {{generic struct cannot be declared public because its generic parameter uses a private type}}
public struct PublicGenericIPReq<T: InternalProto where T: PrivateProto> {} // expected-error {{generic struct cannot be declared public because its generic requirement uses a private type}}

public func genericFunc<T: InternalProto>(_: T) {} // expected-error {{function cannot be declared public because its generic parameter uses an internal type}} {}
public class GenericClass<T: InternalProto> { // expected-error {{generic class cannot be declared public because its generic parameter uses an internal type}}
  public init<T: PrivateProto>(_: T) {} // expected-error {{initializer cannot be declared public because its generic parameter uses a private type}}
  public func genericMethod<T: PrivateProto>(_: T) {} // expected-error {{instance method cannot be declared public because its generic parameter uses a private type}}
}
public enum GenericEnum<T: InternalProto> { // expected-error {{generic enum cannot be declared public because its generic parameter uses an internal type}}
  case A
}


public protocol PublicMutationOperations {
  var size: Int { get set }
  subscript (Int) -> Int { get set }
}

internal protocol InternalMutationOperations {
  var size: Int { get set }
  subscript (Int) -> Int { get set }
}

public struct AccessorsControl : InternalMutationOperations {
  private var size = 0 // expected-error {{property 'size' must be declared internal because it matches a requirement in internal protocol 'InternalMutationOperations'}}
  private subscript (Int) -> Int { // expected-error {{subscript must be declared internal because it matches a requirement in internal protocol 'InternalMutationOperations'}}
    get { return 42 }
    set {}
  }
}

public struct PrivateSettersPublic : InternalMutationOperations {
  // Please don't change the formatting here; it's a precise fix-it test.
  public private(set) var size = 0 // expected-error {{setter for property 'size' must be declared internal because it matches a requirement in internal protocol 'InternalMutationOperations'}} {{10-17=internal}}
  public private(set) subscript (Int) -> Int { // expected-error {{subscript setter must be declared internal because it matches a requirement in internal protocol 'InternalMutationOperations'}} {{10-17=internal}}
    get { return 42 }
    set {}
  }
}

internal struct PrivateSettersInternal : PublicMutationOperations {
  // Please don't change the formatting here; it's a precise fix-it test.
  private(set)var size = 0 // expected-error {{setter for property 'size' must be as accessible as its enclosing type because it matches a requirement in protocol 'PublicMutationOperations'}} {{3-15=}}

  internal private(set)subscript (Int) -> Int { // expected-error {{subscript setter must be as accessible as its enclosing type because it matches a requirement in protocol 'PublicMutationOperations'}} {{12-24=}}
    get { return 42 }
    set {}
  }
}

public protocol PublicReadOnlyOperations {
  var size: Int { get }
  subscript (Int) -> Int { get }
}

internal struct PrivateSettersForReadOnlyInternal : PublicReadOnlyOperations {
  public private(set) var size = 0 // expected-warning {{declaring a public var for an internal struct}}
  internal private(set) subscript (Int) -> Int { // no-warning
    get { return 42 }
    set {}
  }
}

public struct PrivateSettersForReadOnlyPublic : PublicReadOnlyOperations {
  public private(set) var size = 0 // no-warning
  internal private(set) subscript (Int) -> Int { // expected-error {{subscript must be declared public because it matches a requirement in public protocol 'PublicReadOnlyOperations'}}
    get { return 42 }
    set {}
  }
}

