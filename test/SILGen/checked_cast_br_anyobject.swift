// RUN: %target-swift-emit-silgen -module-name checked_cast_br_anyobject -parse-as-library -Xllvm -sil-full-demangle -enforce-exclusivity=checked %s

public struct BridgedSwiftObject {
    var swiftMetatype : UnsafeRawPointer
    var refCounts : Int64
}

public typealias SwiftObject = UnsafeMutablePointer<BridgedSwiftObject>

extension UnsafeMutablePointer where Pointee == BridgedSwiftObject {
  init<T: AnyObject>(_ object: T) {
    let ptr = Unmanaged.passUnretained(object).toOpaque()
    self = ptr.bindMemory(to: BridgedSwiftObject.self, capacity: 1)
  }
  
  func getAs<T: AnyObject>(_ objectType: T.Type) -> T {
    return Unmanaged<T>.fromOpaque(self).takeUnretainedValue()
  }
}

extension Optional where Wrapped == UnsafeMutablePointer<BridgedSwiftObject> {
  func getAs<T: AnyObject>(_ objectType: T.Type) -> T? {
    if let pointer = self {
      return Unmanaged<T>.fromOpaque(pointer).takeUnretainedValue()
    }
    return nil
  }
}

public class Klass {}
public class Klass2 {}

// Make sure that we do not crash when emitting this code!
public func getValue(_ obj: UnsafeMutablePointer<BridgedSwiftObject>) -> AnyObject {
    let v = obj.getAs(AnyObject.self)
    switch v {
    case let k as Klass:
        return k
    case let k as Klass2:
        return k
    default:
        fatalError("unknown type")
    }
}
