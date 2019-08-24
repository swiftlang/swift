// RUN: %target-swift-frontend %s -emit-ir

struct DefaultAssociatedType {
}

protocol Protocol {
    associatedtype AssociatedType = DefaultAssociatedType
    init(object: AssociatedType)
}

final class Conformance: Protocol {
    private let object: AssociatedType
    init(object: AssociatedType) {
        self.object = object
    }
}
