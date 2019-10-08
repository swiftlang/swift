// RUN: %target-typecheck-verify-swift

struct SomeOptions: OptionSet {
    var rawValue: Int
    
    let someVal = MyOptions(rawValue: 6)
    static let some = MyOptions(rawValue: 4)
    static let empty = SomeOptions(rawValue: 0) // expected-warning {{static property 'empty' produces an empty option set}} expected-note {{use [] to silence this warning}}{{35-48=([])}}
    static var otherVal = SomeOptions(rawValue: 0)
}

struct MyOptions: OptionSet {
    let rawValue: Int
    
    static let none = MyOptions(rawValue: 0) // expected-warning {{static property 'none' produces an empty option set}} expected-note {{use [] to silence this warning}}{{32-45=([])}}
    static var nothing = MyOptions(rawValue: 0)
    static let nope = MyOptions()
    static let other = SomeOptions(rawValue: 8)
}
