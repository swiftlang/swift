
import Submodule
import PrivateCModule

public func incrementByThree(_ x: Int) -> Int {
  return incrementByOne(x) + 2
}

public func incrementByThreeWithCall(_ x: Int) -> Int {
  return incrementByOneNoCMO(x) + 2
}

public func submoduleKlassMember() -> Int {
  let k = SubmoduleKlass()
  return k.i
}

public final class ModuleKlass {
  public var i: Int

  public init() {
    i = 27
  }
}

public func moduleKlassMember() -> Int {
  let k = ModuleKlass()
  return k.i
}

public struct ModuleStruct {
  public static var publicFunctionPointer: (Int) -> (Int) = incrementByThree
  public static var privateFunctionPointer: (Int) -> (Int) = { $0 }
}

public func callPrivateCFunc() -> Int {
  return Int(privateCFunc())
}

public func usePrivateCVar() -> Int {
  return Int(privateCVar);
}
