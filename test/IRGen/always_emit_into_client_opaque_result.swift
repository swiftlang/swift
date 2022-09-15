// RUN: %target-swift-frontend -enable-library-evolution -emit-ir %s

// REQUIRES: OS=macosx

// This test used to crash with a duplicate LLVM IR definition.

@available(macOS 11.0, *)
public protocol Proto1 {}

@available(macOS 11.0, *)
public struct Thing {}

@available(macOS 11.0, *)
public struct Thing0: Proto1 {
    public init() {}
}
@available(macOS 11.0, *)
@_marker public protocol MarkerProto {}

@available(macOS 11.0, *)
@frozen
@usableFromInline
struct LimitedAvailability: Proto1, MarkerProto {}

@available(macOS 11.0, *)
extension Thing {
    @_alwaysEmitIntoClient
    public static func doIt(
        _ thingy: (any Proto1 & MarkerProto)?
    ) -> some Proto1 {
        if #available(macOS 13.0, *) {
            return thingy as! LimitedAvailability
        } else {
            return Thing0()
        }
    }
}

@available(macOS 11.0, *)
public  func doIt(_ thingy: (any Proto1 & MarkerProto)?) -> some Proto1 {
    return Thing.doIt(thingy)
}
