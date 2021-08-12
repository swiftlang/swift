public extension MyStruct {
    var propertyInExtension: String { "" }
    func funcInExtension() -> String { "" }

    func addedMethod() -> MyStruct { return self }
}
