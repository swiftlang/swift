public struct Thing {
    public var name: String

    public init(
        name: String,
        flag: Bool = true,
        items: [String] = [],
        opt: String? = nil,
        locallyPassed: Bool = true
    ) {
        self.name = name
    }
}
