// RUN: %swift -parse %s -verify

// Test casting through a class type to a bridged value type.

class BridgedClass { 
  // FIXME: We shouldn't depend on these implicit conversions.
 
  @conversion func __conversion() -> BridgedStruct {
    return BridgedStruct()
  }
}

struct BridgedStruct : _BridgedToObjectiveC {
  static func getObjectiveCType() -> Any.Type {
    return BridgedClass.self
  }

  func bridgeToObjectiveC() -> BridgedClass {
    return BridgedClass()
  }

  static func bridgeFromObjectiveC(x: BridgedClass) -> BridgedStruct? {
    return nil
  }
}

func testBridgeDowncast(obj: AnyObject, objOpt: AnyObject?, 
                        objImplicitOpt: AnyObject!) -> BridgedStruct? {
  var s1Opt = obj as? BridgedStruct
  var s2Opt = objOpt as? BridgedStruct
  var s3Opt = objImplicitOpt as? BridgedStruct

  // Make sure we seem to have the right result type.
  if s1Opt { return s1Opt }
  s2Opt = s1Opt
  s2Opt = s3Opt
  s3Opt = s1Opt

  return s1Opt
}

func testBridgeIsa(obj: AnyObject, objOpt: AnyObject?, 
                   objImplicitOpt: AnyObject!) {
  if obj is BridgedStruct { }
  if objOpt is BridgedStruct { }
  if objImplicitOpt is BridgedStruct { }
}
