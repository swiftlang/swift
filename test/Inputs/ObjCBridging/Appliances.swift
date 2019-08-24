@_exported import Appliances

public struct Refrigerator {
  public var temperature: Double
}

extension Refrigerator : _ObjectiveCBridgeable {
  public typealias _ObjectiveCType = APPRefrigerator

  public func _bridgeToObjectiveC() -> _ObjectiveCType {
    return APPRefrigerator(temperature: temperature)
  }

  public static func _forceBridgeFromObjectiveC(
    _ source: _ObjectiveCType,
    result: inout Refrigerator?
  ) {
    result = Refrigerator(temperature: source.temperature)
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ source: _ObjectiveCType,
    result: inout Refrigerator?
  ) -> Bool {
    result = Refrigerator(temperature: source.temperature)
    return true
  }

  public static func _unconditionallyBridgeFromObjectiveC(_ source: _ObjectiveCType?)
      -> Refrigerator {
    return Refrigerator(temperature: source!.temperature)
  }
}

public struct ManufacturerInfo<DataType: AnyObject> {
  fileprivate var impl: APPManufacturerInfo<DataType>
  public var value: DataType {
    return impl.value
  }
}

extension ManufacturerInfo : _ObjectiveCBridgeable {
  public typealias _ObjectiveCType = APPManufacturerInfo<DataType>

  public func _bridgeToObjectiveC() -> _ObjectiveCType {
    return impl
  }

  public static func _forceBridgeFromObjectiveC(
    _ source: _ObjectiveCType,
    result: inout ManufacturerInfo?
  ) {
    result = ManufacturerInfo(impl: source)
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ source: _ObjectiveCType,
    result: inout ManufacturerInfo?
  ) -> Bool {
    result = ManufacturerInfo(impl: source)
    return true
  }

  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: _ObjectiveCType?
  ) -> ManufacturerInfo {
    return ManufacturerInfo(impl: source!)
  }
}
