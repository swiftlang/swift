// RUN: %target-swift-frontend -typecheck %s

public class Foo : CustomReflectable  {
  public var booleanValue : Bool?
  public var customMirror: Mirror {
    return Mirror(self, children: [
        "booleanValue": booleanValue as Bool? as Any,
        "booleanValue": booleanValue as Bool? as Any,
        "booleanValue": booleanValue as Bool? as Any,
        "booleanValue": booleanValue as Bool? as Any,
        "booleanValue": booleanValue as Bool? as Any,
        "booleanValue": booleanValue as Bool? as Any,
        "booleanValue": booleanValue as Bool? as Any,
        "booleanValue": booleanValue as Bool? as Any,
        "booleanValue": booleanValue as Bool? as Any,
        "booleanValue": booleanValue as Bool? as Any
      ])
  }
}
