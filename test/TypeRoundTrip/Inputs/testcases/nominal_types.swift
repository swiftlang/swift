import RoundTrip

struct Outer  {
  enum Inner {
    case a
    init() { fatalError() }
  }
  enum GenericInner<T, U> {
    case a
    init() { fatalError() }
  }
}

enum GenericOuter<T, U> {
  case a
  init() { fatalError() }

  struct Inner {}
  struct GenericInner<T, U> {}
  struct InnerWhere where T == GenericOuter<U, U> {}
}

protocol P {}

struct ImplementsP: P {}

struct Constrained<T : P> {}

func generic<T>(_: Constrained<T>) {}


protocol STSTagProtocol {}
struct STSOuter : STSTagProtocol {}

enum STSContainer<T : STSTagProtocol> {
  class Superclass {}
  class Subclass<U>: Superclass where T == STSOuter {
    class ExtraNested: Superclass {}
    class ExtraNestedWhere: Superclass where U: Subclass<T> {}
  }

  class GenericSuperclass<U> {}
  class Subclass2<U>: GenericSuperclass<U> where T == STSOuter {}

  class Subclass3<U: Collection>: Superclass where T == U.Element {}

  class MoreNesting<X> {
    class Subclass<U>: Superclass where T == STSOuter {}
  }

  struct Fields<U> where T == STSOuter {
    var x: T?
    var y: U?
  }

  enum Cases<U> where T == STSOuter {
    case a(T)
    case b(U)
  }
}

// A new type with an easily-recognizable, easily-strippable suffix character.
enum STSContainer℠<T : STSTagProtocol> {
  class Superclass {}
  class GenericSuperclass<U> {}
}
extension STSContainer℠ where T == STSOuter {
  class Subclass<U>: Superclass {
    class ExtraNested: Superclass {}
  }

  class Subclass2<U>: GenericSuperclass<U> {}

  class MoreNesting<X> {
    class Subclass<U>: Superclass {}
  }

  struct Fields<U> {
    var x: T?
    var y: U?
  }

  enum Cases<U> {
    case a(T)
    case b(U)
  }
}

public func test() {
  roundTripType(Outer.self)
  roundTripType(Outer.Inner.self)
  roundTripType(Outer.GenericInner<Int, String>.self)
  roundTripType(Outer.GenericInner<UInt, Double>.self)

  roundTripType(GenericOuter<Int, Float>.Inner.self)
  roundTripType(GenericOuter<UInt, String>.GenericInner<Float, Double>.self)
  roundTripType(GenericOuter<GenericOuter<Bool, Bool>, Bool>.InnerWhere.self)

  roundTripType(GenericOuter<Int, String>.self)

  roundTripType(Constrained<ImplementsP>.self)

  roundTripType(STSContainer<STSOuter>.Subclass<Int>.self)
  roundTripType(STSContainer℠<STSOuter>.Subclass<Int>.self)

  roundTripType(STSContainer<STSOuter>.Subclass2<Int>.self)
  roundTripType(STSContainer℠<STSOuter>.Subclass2<Int>.self)

  roundTripType(STSContainer<STSOuter>.Subclass3<Array<STSOuter>>.self)
  roundTripType(STSContainer<STSOuter>.Subclass<Int>.ExtraNested.self)
  roundTripType(STSContainer<STSOuter>.Subclass<STSContainer<STSOuter>.Subclass<STSOuter>>.ExtraNestedWhere.self)
  roundTripType(STSContainer℠<STSOuter>.Subclass<Int>.ExtraNested.self)
  roundTripType(STSContainer<STSOuter>.MoreNesting<Bool>.Subclass<Int>.self)
  roundTripType(STSContainer℠<STSOuter>.MoreNesting<Bool>.Subclass<Int>.self)

  roundTripType(STSContainer<STSOuter>.Fields<Int>.self)
  roundTripType(STSContainer℠<STSOuter>.Fields<Int>.self)

  roundTripType(STSContainer<STSOuter>.Cases<Int>.self)
  roundTripType(STSContainer℠<STSOuter>.Cases<Int>.self)
}
