import Foundation

final class ObjcClass : NSObject {
  fileprivate var ii: Int = 127
}

@inline(never)
func returnObjcClassMember<T>(_ c: ObjcClass, _ t: T) -> Int {
  return c.ii
}

@inline(never)
public func callObjcClassMember<T>(_ t: T) -> Int {
  let c = ObjcClass()
  return returnObjcClassMember(c, t)
}

