// RUN: %target-typecheck-verify-swift

struct SomeOptions: OptionSet {
    var rawValue: Int
    static let some = MyOptions(rawValue: 4)
    let empty = SomeOptions(rawValue: 0)
    var otherVal = SomeOptions(rawValue: 0)
}

struct MyOptions: OptionSet {
    init() {
        rawValue = 0
    }
    var rawValue: Int
    static let none = MyOptions(rawValue: 0) // expected-warning {{static property 'none' produces an empty option set}} expected-note {{use [] to silence this warning}}{{32-45=([])}}
    static var nothing = MyOptions(rawValue: 0)
    static let nope = MyOptions()
    static let other = SomeOptions(rawValue: 8)
}
