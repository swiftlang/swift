// RUN: %target-typecheck-verify-swift

struct DefaultAssociatedType {
}

protocol Protocol {
    associatedtype AssociatedType = DefaultAssociatedType
    init(object: AssociatedType)
}

final class Conformance: Protocol {
    private let object: AssociatedType
    init(object: AssociatedType) { // expected-error {{reference to invalid associated type 'AssociatedType' of type 'Conformance'}}
        self.object = object
    }
}
