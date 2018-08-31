protocol DefaultedAttributes: Attributes {
}

extension DefaultedAttributes {
    var attributes: [(title: String, description: String)] {
        var attributes: [(title: String, description: String)] = []
        return attributes
    }
}

extension Impl: DefaultedAttributes {
}
