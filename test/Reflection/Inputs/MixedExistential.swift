import Foundation

@objc public protocol ObjCProto {}

public protocol ClassConstrainedProto: AnyObject {
    var payload: Int { get }
}

public protocol UnconstrainedProto {
    var payload: Int { get }
}

public struct MixedWithClassConstrainedProto {
    public var existential: ObjCProto & ClassConstrainedProto
    public init(existential: ObjCProto & ClassConstrainedProto) {
        self.existential = existential
    }
}

public struct MixedWithUnconstrainedProto {
    public var existential: ObjCProto & UnconstrainedProto
    public init(existential: ObjCProto & UnconstrainedProto) {
        self.existential = existential
    }
}

public struct MixedExistentialMetatype {
    public var metatype: (ObjCProto & UnconstrainedProto).Type
    public init(metatype: (ObjCProto & UnconstrainedProto).Type) {
        self.metatype = metatype
    }
}
