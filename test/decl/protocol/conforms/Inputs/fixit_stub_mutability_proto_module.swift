public protocol ExternalMutabilityProto {
    mutating func foo()
    subscript() -> Int { mutating get nonmutating set }
}
