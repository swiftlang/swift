// RUN: %target-typecheck-verify-swift

struct OuterNonGeneric {
  struct MidNonGeneric {
    struct InnerNonGeneric {}
    struct InnerGeneric<A> {}
  }

  struct MidGeneric<B> {
    struct InnerNonGeneric {}
    struct InnerGeneric<C> {}

    func flock(_ b: B) {}
  }
}

struct OuterGeneric<D> {
  struct MidNonGeneric {
    struct InnerNonGeneric {}
    struct InnerGeneric<E> {}

    func roost(_ d: D) {}
  }

  struct MidGeneric<F> {
    struct InnerNonGeneric {}
    struct InnerGeneric<G> {}

    func nest(_ d: D, f: F) {}
  }

  func nonGenericMethod(_ d: D) {
    func genericFunction<E>(_ d: D, e: E) {}

    genericFunction(d, e: ())
  }
}

class OuterNonGenericClass {
  enum InnerNonGeneric {
    case Baz
    case Zab
  }

  class InnerNonGenericBase {
    init() {}
  }

  class InnerNonGenericClass1 : InnerNonGenericBase {
    override init() {
      super.init()
    }
  }

  class InnerNonGenericClass2 : OuterNonGenericClass {
    override init() {
      super.init()
    }
  }

  class InnerGenericClass<U> : OuterNonGenericClass {
    override init() {
      super.init()
    }
  }
}

class OuterGenericClass<T> {
  enum InnerNonGeneric {
    case Baz
    case Zab
  }

  class InnerNonGenericBase {
    init() {}
  }

  class InnerNonGenericClass1 : InnerNonGenericBase {
    override init() {
      super.init()
    }
  }

  class InnerNonGenericClass2 : OuterGenericClass {
    override init() {
      super.init()
    }
  }

  class InnerNonGenericClass3 : OuterGenericClass<Int> {
    override init() {
      super.init()
    }
  }

  class InnerNonGenericClass4 : OuterGenericClass<T> {
    override init() {
      super.init()
    }
  }

  class InnerGenericClass<U> : OuterGenericClass<U> {
    override init() {
      super.init()
    }
  }

  class Middle {
    class Inner1<T> {}
    class Inner2<T> : Middle where T: Inner1<Int> {}
  }
}

// <rdar://problem/12895793>
struct AnyStream<T : Sequence> {
  struct StreamRange<S : IteratorProtocol> {
    var index : Int
    var elements : S

    // Conform to the IteratorProtocol protocol.
    typealias Element = (Int, S.Element)
    mutating
    func next() -> Element? {
      let result = (index, elements.next())
      if result.1 == nil { return .none }
      index += 1
      return (result.0, result.1!)
    }
  }

  var input : T

  // Conform to the enumerable protocol.
  typealias Elements = StreamRange<T.Iterator>
  func getElements() -> Elements {
    return Elements(index: 0, elements: input.makeIterator())
  }
}

func enumerate<T>(_ arg: T) -> AnyStream<T> {
  return AnyStream<T>(input: arg)
}

// Check unqualified lookup of inherited types.
class Foo<T> {
  typealias Nested = T
}

class Bar : Foo<Int> {
  func f(_ x: Int) -> Nested {
    return x
  }

  struct Inner {
    func g(_ x: Int) -> Nested {
      return x
    }

    func withLocal() {
      struct Local {
        func h(_ x: Int) -> Nested {
          return x
        }
      }
    }
  }
}

extension Bar {
  func g(_ x: Int) -> Nested {
    return x
  }

  // <rdar://problem/14376418>
  struct Inner2 {
    func f(_ x: Int) -> Nested {
      return x
    }
  }
}

class X6<T> {
  let d: D<T>
  init(_ value: T) {
    d = D(value)
  }
  class D<T2> {
    init(_ value: T2) {}
  }
}

// ---------------------------------------------
// Unbound name references within a generic type
// ---------------------------------------------
struct GS<T> {
  func f() -> GS {
    let gs = GS()
    return gs
  }

  struct Nested {
    func ff() -> GS {
      let gs = GS()
      return gs
    }
  }

  struct NestedGeneric<U> { // expected-note{{generic type 'NestedGeneric' declared here}}
    func fff() -> (GS, NestedGeneric) {
      let gs = GS()
      let ns = NestedGeneric()
      return (gs, ns)
    }
  }

  // FIXME: We're losing some sugar here by performing the substitution.
  func ng() -> NestedGeneric { } // expected-error{{reference to generic type 'GS<T>.NestedGeneric' requires arguments in <...>}}
}

extension GS {
  func g() -> GS {
    let gs = GS()
    return gs
  }

  func h() {
    _ = GS() as GS<Int> // expected-error{{'GS<T>' is not convertible to 'GS<Int>'; did you mean to use 'as!' to force downcast?}}
  }
}

struct HasNested<T> { // expected-note {{arguments to generic parameter 'T' ('Int' and 'Float') are expected to be equal}}
// expected-note@-1 {{arguments to generic parameter 'T' ('Float' and 'Int') are expected to be equal}}
  init<U>(_ t: T, _ u: U) {}
  func f<U>(_ t: T, u: U) -> (T, U) {}

  struct InnerGeneric<U> {
    init() {}
    func g<V>(_ t: T, u: U, v: V) -> (T, U, V) {}
  }

  struct Inner {
    init (_ x: T) {}
    func identity(_ x: T) -> T { return x }
  }
}

func useNested(_ ii: Int, hni: HasNested<Int>,
               xisi : HasNested<Int>.InnerGeneric<String>,
               xfs: HasNested<Float>.InnerGeneric<String>) {
  var i = ii, xis = xisi
  typealias InnerI = HasNested<Int>.Inner
  var innerI = InnerI(5)
  typealias InnerF = HasNested<Float>.Inner
  var innerF : InnerF = innerI // expected-error{{cannot convert parent type 'HasNested<Int>' to expected type 'HasNested<Float>'}}

  _ = innerI.identity(i)
  i = innerI.identity(i)

  // Generic function in a generic class
  typealias HNI = HasNested<Int>
  var id = hni.f(1, u: 3.14159)
  id = (2, 3.14159)
  hni.f(1.5, 3.14159) // expected-error{{missing argument label 'u:' in call}}
  hni.f(1.5, u: 3.14159) // expected-error{{cannot convert value of type 'Double' to expected argument type 'Int'}}

  // Generic constructor of a generic struct
  HNI(1, 2.71828) // expected-warning{{unused}}
  HNI(1.5, 2.71828) // expected-error{{'Double' is not convertible to 'Int'}}

  // Generic function in a nested generic struct
  var ids = xis.g(1, u: "Hello", v: 3.14159)
  ids = (2, "world", 2.71828)

  xis = xfs // expected-error{{cannot convert parent type 'HasNested<Float>' to expected type 'HasNested<Int>'}}
}

// Extensions of nested generic types
extension OuterNonGeneric.MidGeneric {
  func takesB(b: B) {}
}

extension OuterGeneric.MidNonGeneric {
  func takesD(d: D) {}
}

extension OuterGeneric.MidGeneric {
  func takesD(d: D) {}
  func takesB(f: F) {}
}

protocol HasAssocType {
  associatedtype FirstAssocType
  associatedtype SecondAssocType

  func takesAssocType(first: FirstAssocType, second: SecondAssocType)
}

extension OuterGeneric.MidGeneric : HasAssocType {
  func takesAssocType(first: D, second: F) {}
}

typealias OuterGenericMidNonGeneric<T> = OuterGeneric<T>.MidNonGeneric

extension OuterGenericMidNonGeneric {

}

class BaseClass {
  struct T {}

  func m1() -> T {}
  func m2() -> BaseClass.T {}
  func m3() -> DerivedClass.T {}
}

func f1() -> DerivedClass.T {
  return BaseClass.T()
}

func f2() -> BaseClass.T {
  return DerivedClass.T()
}

func f3() -> DerivedClass.T {
  return DerivedClass.T()
}

class DerivedClass : BaseClass {
  override func m1() -> DerivedClass.T {
    return f2()
  }

  override func m2() -> BaseClass.T {
    return f3()
  }

  override func m3() -> T {
    return f2()
  }
}

// https://bugs.swift.org/browse/SR-3847: Resolve members in inner types.
// This first extension isn't necessary; we could have put 'originalValue' in
// the original declaration.
extension OuterNonGenericClass.InnerNonGenericBase {
  static let originalValue = 0
}
// Each of these two cases used to crash.
extension OuterNonGenericClass.InnerNonGenericBase {
  static let propUsingMember = originalValue
}
extension OuterNonGenericClass.InnerNonGenericClass1 {
  static let anotherPropUsingMember = originalValue
}

// rdar://problem/30353095: Extensions of nested types with generic
// requirements placed on type parameters
struct OuterWithConstraint<T : HasAssocType> {
  struct InnerWithConstraint<U : HasAssocType> { }
}

extension OuterWithConstraint.InnerWithConstraint {
  func foo<V>(v: V) where T.FirstAssocType == U.SecondAssocType {}
}

// Name lookup within a 'where' clause should find generic parameters
// of the outer type.
extension OuterGeneric.MidGeneric where D == Int, F == String {
  func doStuff() -> (D, F) {
    return (100, "hello")
  }
}

// https://bugs.swift.org/browse/SR-4672
protocol ExpressibleByCatLiteral {}
protocol ExpressibleByDogLiteral {}

struct Kitten : ExpressibleByCatLiteral {}
struct Puppy : ExpressibleByDogLiteral {}

struct Claws<A: ExpressibleByCatLiteral> { // expected-note 3 {{'A' declared as parameter to type 'Claws'}}
  struct Fangs<B: ExpressibleByDogLiteral> { } // expected-note {{where 'B' = 'NotADog'}}
}

struct NotADog {}

func pets<T>(fur: T) -> Claws<Kitten>.Fangs<T> {
  return Claws<Kitten>.Fangs<T>()
}

func something<T>() -> T {
  while true {}
}

func test() {
  let _: Claws<Kitten>.Fangs<Puppy> = pets(fur: Puppy())

  // <https://bugs.swift.org/browse/SR-5600>
  let _: Claws.Fangs<Puppy> = pets(fur: Puppy())
  let _: Claws.Fangs<Puppy> = Claws<Kitten>.Fangs()
  let _: Claws.Fangs<Puppy> = Claws.Fangs()
  // expected-error@-1 {{generic parameter 'A' could not be inferred}}
  // expected-note@-2 {{explicitly specify the generic arguments to fix this issue}} {{36-36=<<#A: ExpressibleByCatLiteral#>>}}
  let _: Claws.Fangs<NotADog> = something()
  // expected-error@-1 {{generic parameter 'A' could not be inferred}}
  _ = Claws.Fangs<NotADog>()
  // expected-error@-1 {{generic parameter 'A' could not be inferred}}
  // expected-error@-2 {{generic struct 'Fangs' requires that 'NotADog' conform to 'ExpressibleByDogLiteral'}}
  // expected-note@-3 {{explicitly specify the generic arguments to fix this issue}} {{12-12=<<#A: ExpressibleByCatLiteral#>>}}
}

// https://bugs.swift.org/browse/SR-4379
extension OuterGeneric.MidNonGeneric {
  func doStuff() -> OuterGeneric {
    return OuterGeneric()
  }

  func doMoreStuff() -> OuterGeneric.MidNonGeneric {
    return OuterGeneric.MidNonGeneric()
  }

  func doMoreStuffWrong() -> Self {

  }
}
