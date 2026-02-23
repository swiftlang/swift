// RUN: %target-swift-frontend %s -O -module-name=test -disable-availability-checking -emit-sil | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// CHECK-LABEL: sil @$s4test12getAnyObject4from3keyyXlSgSDySOypG_SOtF :
// CHECK-NOT:     alloc_stack
// CHECK-NOT:     copy_addr
// CHECK:       } // end sil function '$s4test12getAnyObject4from3keyyXlSgSDySOypG_SOtF'
public func getAnyObject(from dict: Dictionary<ObjectIdentifier, Any>, key: ObjectIdentifier) -> AnyObject? {
  getValue(from: dict, forKey: key, as: AnyObject.self)
}

func getValue<Value>(from dict: Dictionary<ObjectIdentifier, Any>, forKey key: ObjectIdentifier, as: Value.Type) -> Value? {
  @_transparent func project<T>( t: consuming T) -> Value {
    unsafeBitCast(t, to: Value.self)
  }
  if let value = dict[key] {
    return _openExistential(consume value, do: project)
  } else {
    return nil
  }
}

