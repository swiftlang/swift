protocol Attributed {
    var asAttributes: DefaultedAttributes? { get }
}

extension Attributed {
    var asAttributes: DefaultedAttributes? { return self as? DefaultedAttributes }
}

struct Impl: Attributed {
}
