public class C1<T> : ClassBoundP {
  public typealias Function = (C1<T>) -> (S1<T>) -> (E1<T>) -> Int
  public typealias Tuple = (C1<T>, S1<T>, E1<T>, Int)
  public typealias Inner = T
  public let aClass: C1<T>
  public let aStruct: S1<T>
  public let anEnum: E1<T>
  public let function: Function
  public let tuple: Tuple
  public let dependentMember: T

  public init(aClass: C1<T>, aStruct: S1<T>, anEnum: E1<T>, function: Function, tuple: Tuple, dependentMember: T) {
    self.aClass = aClass
    self.aStruct = aStruct
    self.anEnum = anEnum
    self.function = function
    self.tuple = tuple
    self.dependentMember = dependentMember
  }
}

public class C2<T: P1> {
  public typealias Function = (C1<T>) -> (S1<T>) -> (E1<T>) -> Int
  public typealias Tuple = (C2<T>, S2<T>, E2<T>, Int)
  public let aClass: C1<T>
  public let aStruct: S1<T>
  public let anEnum: E1<T>
  public let function: Function
  public let tuple: Tuple
  public let primaryArchetype: T
  public let dependentMember1: T.Inner
  public init(aClass: C1<T>, aStruct: S1<T>, anEnum: E1<T>, function: Function, tuple: Tuple, primaryArchetype: T, dependentMember1: T.Inner) {
    self.aClass = aClass
    self.aStruct = aStruct
    self.anEnum = anEnum
    self.function = function
    self.tuple = tuple
    self.primaryArchetype = primaryArchetype
    self.dependentMember1 = dependentMember1
  }
}

public class C3<T: P2> {
  public typealias Function = (C3<T>) -> (S3<T>) -> (E3<T>) -> Int
  public typealias Tuple = (C3<T>, S3<T>, E3<T>, Int)
  public let aClass: C3<T>
  public let aStruct: S3<T>
  public let anEnum: E3<T>
  public let function: (C3<T>) -> (S3<T>) -> (E3<T>) -> Int
  public let tuple: (C3<T>, S3<T>, E3<T>, Int)
  public let primaryArchetype: T
  public let dependentMember1: T.Outer
  public let dependentMember2: T.Outer.Inner
  public init(aClass: C3<T>, aStruct: S3<T>, anEnum: E3<T>, function: Function, tuple: Tuple, primaryArchetype: T, dependentMember1: T.Outer, dependentMember2: T.Outer.Inner) {
    self.aClass = aClass
    self.aStruct = aStruct
    self.anEnum = anEnum
    self.function = function
    self.tuple = tuple
    self.primaryArchetype = primaryArchetype
    self.dependentMember1 = dependentMember1
    self.dependentMember2 = dependentMember2
  }
}

public struct C4<T : P1, U : P1> {}
extension C4 : P1, P2 {
  public typealias Inner = T
  public typealias Outer = T
}

public struct S1<T> {
  public let aClass: C1<T>
  public let aStruct: Box<S1<T>>
  public let anEnum: Box<E1<T>>
  public let function: (C1<T>) -> (S1<T>) -> (E1<T>) -> Int
  public let tuple: (C1<T>, Box<S1<T>>, Box<E1<T>>, Int)
  public let primaryArchetype: T
}

public struct S2<T: P1> {
  public let aClass: C2<T>
  public let aStruct: Box<S2<T>>
  public let anEnum: Box<E2<T>>
  public let function: (C2<T>) -> (S2<T>) -> (E2<T>) -> Int
  public let tuple: (C2<T>, Box<S2<T>>, Box<E2<T>>, Int)
  public let primaryArchetype: T
  public let dependentMember1: T.Inner
}

public struct S3<T: P2> {
  public typealias Function = (C3<T>) -> (S3<T>) -> (E3<T>) -> Int
  public typealias Tuple = (C3<T>, Box<S3<T>>, Box<E3<T>>, Int)
  public let aClass: C3<T>
  public let aStruct: Box<S3<T>>
  public let anEnum: Box<E3<T>>
  public let function: Function
  public let tuple: Tuple
  public let primaryArchetype: T
  public let dependentMember1: T.Outer
  public let dependentMember2: T.Outer.Inner
  public init(aClass: C3<T>, aStruct: Box<S3<T>>, anEnum: Box<E3<T>>, function: Function, tuple: Tuple, primaryArchetype: T, dependentMember1: T.Outer, dependentMember2: T.Outer.Inner) {
    self.aClass = aClass
    self.aStruct = aStruct
    self.anEnum = anEnum
    self.function = function
    self.tuple = tuple
    self.primaryArchetype = primaryArchetype
    self.dependentMember1 = dependentMember1
    self.dependentMember2 = dependentMember2
  }
}

public struct S4<T : P1, U : P1> {}
extension S4 : P1, P2 {
  public typealias Inner = T
  public typealias Outer = T
}

public enum E1<T> {
  case Class(C1<T>)
  case Struct(S1<T>)
  indirect case Enum(E1<T>)
  case Int(Swift.Int)
  case Function((T) -> E1<T>)
  case Tuple(C1<T>, S1<T>, Swift.Int)
  case Primary(T)
  case Metatype(T.Type)
}

public enum E2<T: P1> {
  case Class(C2<T>)
  case Struct(S2<T>)
  indirect case Enum(E2<T>)
  case Function((T.Type) -> E1<T>)
  case Tuple(C2<T>, S2<T>, Int)
  case Primary(T)
  case DependentMemberInner(T.Inner)
  case ExistentialMetatype(T.Type)
}

public enum E3<T: P2> {
  case Class(C3<T>)
  case Struct(S3<T>)
  indirect case Enum(E3<T>)
  case Function((T.Type.Type) -> E1<T>)
  case Tuple(C3<T>, S3<T>, Int)
  case Primary(T)
  case DependentMemberOuter(T.Outer)
  case DependentMemberInner(T.Outer.Inner)
}

public enum E4<T : P1, U : P1> {
  public typealias First = T
}
extension E4 : P1, P2, P3 {
  public typealias Inner = T
  public typealias Outer = U
  public typealias Second = U
}
