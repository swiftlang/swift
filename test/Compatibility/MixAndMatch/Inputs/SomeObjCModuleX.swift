// NB: This file is not named SomeObjCModule.swift to avoid getting picked up
// by -enable-source-import

@_exported import SomeObjCModule

public struct RuncibleSpoon: _ObjectiveCBridgeable {
  public init() {}

  public func _bridgeToObjectiveC() -> NSRuncibleSpoon {
    fatalError()
  }
  public static func _forceBridgeFromObjectiveC(_: NSRuncibleSpoon, result: inout RuncibleSpoon?) {
    fatalError()
  }
  public static func _conditionallyBridgeFromObjectiveC(_: NSRuncibleSpoon, result: inout RuncibleSpoon?) -> Bool {
    fatalError()
  }
  public static func _unconditionallyBridgeFromObjectiveC(_: NSRuncibleSpoon?) -> RuncibleSpoon {
    fatalError()
  }
}

