import A

public extension SomeStruct {
    struct InnerStruct: Equatable {}
}

public extension SomeStruct.InnerStruct {
    struct NestedStruct: Equatable {}
}
