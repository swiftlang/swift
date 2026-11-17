// RUN: %target-swift-frontend -emit-ir %s -language-mode 4

// https://github.com/apple/swift/issues/48395

public struct DefaultAssociatedType {
}

protocol Protocol {
    associatedtype AssociatedType = DefaultAssociatedType
    init(object: AssociatedType)
}

public final class Conformance: Protocol {
    private let object: AssociatedType
    public init(object: AssociatedType) {
        self.object = object
    }
}
