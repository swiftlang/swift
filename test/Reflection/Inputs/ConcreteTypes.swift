#if _runtime(_ObjC)
import Foundation
#endif

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
  public let aMetatype: C.Type
  public let aFunction: (C, S, E, Int) -> (Int)
  public init(aClass: C, aStruct: S, anEnum: E, aTuple: (C, S, E, Int), aMetatype: C.Type, aFunction: (C, S, E, Int) -> Int) {
    self.aClass = aClass
    self.aStruct = aStruct
    self.anEnum = anEnum
    self.aTuple = aTuple
    self.aMetatype = aMetatype
    self.aFunction = aFunction
  }
}

public struct S {
  public let aClass: C
  public let aStruct: Box<S>
  public let anEnum: Box<E>
  public let aTuple: (C, Box<S>, Box<E>, Int)
  public let aMetatype: C.Type
  public let aFunction: (C, S, E, Int) -> (Int)
}

public enum E {
  case Class(C)
  case Struct(S)
  indirect case Enum(E)
  case Function((C,S,E,Int) -> ())
  case Tuple(C, S, Int)
  indirect case IndirectTuple(C, S, E, Int)
  case Metatype(E.Type)
}

#if _runtime(_ObjC)

public class OC : NSObject {
  public let aClass: C
  public let aStruct: S
  public let anEnum: E
  public let aTuple: (C, S, E, Int)
  public let aMetatype: C.Type
  public let aFunction: (C, S, E, Int) -> (Int)
  public let nsObject: NSObject
  public let nsString: NSString
  public let cfString: CFString
  public init(aClass: C, aStruct: S, anEnum: E, aTuple: (C, S, E, Int), aMetatype: C.Type, aFunction: (C, S, E, Int) -> Int, nsObject: NSObject, nsString: NSString, cfString: CFString) {
    self.aClass = aClass
    self.aStruct = aStruct
    self.anEnum = anEnum
    self.aTuple = aTuple
    self.aMetatype = aMetatype
    self.aFunction = aFunction
    self.nsObject = nsObject
    self.nsString = nsString
    self.cfString = cfString
  }
}

#endif
