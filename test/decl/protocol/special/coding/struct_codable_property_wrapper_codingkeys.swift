// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/88459
// Circular reference when property wrapper default value references CodingKeys

@propertyWrapper
struct Wrapper<T: Codable & Equatable>: Codable, Equatable {
    var wrappedValue: T
    init(wrappedValue: T, key: CodingKey) {
        self.wrappedValue = wrappedValue
    }
    init(from decoder: Decoder) throws {
        wrappedValue = try decoder.singleValueContainer().decode(T.self)
    }
    func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(wrappedValue)
    }
}

// Property wrapper referencing CodingKeys in default value should not cause
// a circular reference error.
struct SimpleWrapper: Codable { // no error
    enum CodingKeys: String, CodingKey {
        case foo
        case bar
    }
    @Wrapper(key: CodingKeys.foo)
    var foo: String = "hello"
    @Wrapper(key: CodingKeys.bar)
    var bar: Int = 42
}

// Multiple property wrappers with custom key mappings
struct CustomKeyMapping: Codable { // no error
    enum CodingKeys: String, CodingKey {
        case name = "user_name"
        case age = "user_age"
    }
    @Wrapper(key: CodingKeys.name)
    var name: String = "default"
    @Wrapper(key: CodingKeys.age)
    var age: Int = 0
}

// Mix of wrapped and unwrapped properties
struct MixedProperties: Codable { // no error
    enum CodingKeys: String, CodingKey {
        case wrapped
        case normal
    }
    @Wrapper(key: CodingKeys.wrapped)
    var wrapped: String = "hello"
    var normal: Int = 0
}

// Only Decodable
struct DecodableOnly: Decodable { // no error
    enum CodingKeys: String, CodingKey {
        case value
    }
    @Wrapper(key: CodingKeys.value)
    var value: String = "test"
}

// Only Encodable
struct EncodableOnly: Encodable { // no error
    enum CodingKeys: String, CodingKey {
        case value
    }
    @Wrapper(key: CodingKeys.value)
    var value: String = "test"
}
