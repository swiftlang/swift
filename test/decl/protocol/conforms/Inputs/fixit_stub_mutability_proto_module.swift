public protocol ExternalMutabilityProto {
    mutating func foo()
    subscript() -> Int { get nonmutating set }
}
