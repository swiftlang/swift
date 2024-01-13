// Input for bridged_casts_folding_same_module.swift test case.

open class NSObject {
    public init() {}
}

extension AnyHashable : _ObjectiveCBridgeable {
    public func _bridgeToObjectiveC() -> NSObject {
        return NSObject()
    }
}
