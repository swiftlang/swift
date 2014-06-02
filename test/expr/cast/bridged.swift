// RUN: %swift -parse %s -verify

// Test casting through a class type to a bridged value type.

class NSObject { }

class BridgedClass : NSObject { 
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

func testBridgeDowncastSuperclass(obj: NSObject, objOpt: NSObject?,
                                  objImplicitOpt: NSObject!) 
       -> BridgedStruct? {
  var s1Opt = obj as? BridgedStruct
  var s2Opt = objOpt as? BridgedStruct
  var s3Opt = objImplicitOpt as? BridgedStruct
}

func testBridgeDowncastExact(obj: BridgedClass, objOpt: BridgedClass?,
                             objImplicitOpt: BridgedClass!) -> BridgedStruct? {
  var s1Opt = obj as? BridgedStruct
  var s2Opt = objOpt as? BridgedStruct
  var s3Opt = objImplicitOpt as? BridgedStruct
}
