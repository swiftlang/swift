@propertyWrapper
struct Printed<Value: Codable>: Codable {
    var wrappedValue: Value {
        didSet { print(wrappedValue) }
    }
}

struct Foo: Codable {
    @Printed var bar: Bool = false
}
