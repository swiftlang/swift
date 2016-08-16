public class Box<Treasure> {
  public let item: Treasure
  public init(item: Treasure) {
    self.item = item
  }
}

public class C {
  public let aClass: C
  public let aStruct: S
  public let anEnum: E
  public let aTuple: (C, S, E, Int)
  public let aTupleWithLabels: (a: C, s: S, e: E)
  public let aMetatype: C.Type
  public let aFunction: (C, S, E, Int) -> (Int)
  public let aFunctionWithVarArgs: (C, S...) -> ()
  public init(aClass: C, aStruct: S, anEnum: E, aTuple: (C, S, E, Int), aTupleWithLabels: (a: C, s: S, e: E), aMetatype: C.Type, aFunction: @escaping (C, S, E, Int) -> Int, aFunctionWithVarArgs: @escaping (C, S...) -> ()) {
    self.aClass = aClass
    self.aStruct = aStruct
    self.anEnum = anEnum
    self.aTuple = aTuple
    self.aTupleWithLabels = aTupleWithLabels
    self.aMetatype = aMetatype
    self.aFunction = aFunction
    self.aFunctionWithVarArgs = aFunctionWithVarArgs
  }
}

public struct S {
  public let aClass: C
  public let aStruct: Box<S>
  public let anEnum: Box<E>
  public let aTuple: (C, Box<S>, Box<E>, Int)
  public let aMetatype: C.Type
  public let aFunction: (C, S, E, Int) -> (Int)
  public let aFunctionWithThinRepresentation: @convention(thin) () -> ()
  public let aFunctionWithCRepresentation: @convention(c) () -> ()

  public struct NestedS {
    public let aField: Int
  }
}

public enum E {
  case Class(C)
  case Struct(S)
  indirect case Enum(E)
  case Function((C,S,E,Int) -> ())
  case Tuple(C, S, Int)
  indirect case IndirectTuple(C, S, E, Int)
  case Metatype(E.Type)
  case NestedStruct(S.NestedS)
  case EmptyCase
}

public struct References {
  public let strongRef: C
  public weak var weakRef: C?
  public unowned var unownedRef: C
  public unowned(unsafe) var unownedUnsafeRef: C
}
