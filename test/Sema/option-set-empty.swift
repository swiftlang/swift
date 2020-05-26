// RUN: %target-typecheck-verify-swift

struct SomeOptions: OptionSet {
    var rawValue: Int
    
    static let some = MyOptions(rawValue: 4)
    static let empty = SomeOptions(rawValue: 0) // expected-warning {{static property 'empty' produces an empty option set}} expected-note {{use [] to silence this warning}}{{35-48=([])}}
    static var otherVal = SomeOptions(rawValue: 0)
    
    let someVal = MyOptions(rawValue: 6)
    let option = MyOptions(float: Float.infinity)
    let none = SomeOptions(rawValue: 0) // expected-error {{value type 'SomeOptions' cannot have a stored property that recursively contains it}}
}

struct MyOptions: OptionSet {
    let rawValue: Int
    
    init(rawValue: Int) {
        self.rawValue = rawValue
    }

    init(float: Float) {
        self.rawValue = float.exponent
    }
    
    static let none = MyOptions(rawValue: 0) // expected-warning {{static property 'none' produces an empty option set}} expected-note {{use [] to silence this warning}}{{32-45=([])}}
    static var nothing = MyOptions(rawValue: 0)
    static let nope = MyOptions()
    static let other = SomeOptions(rawValue: 8)
    static let piVal = MyOptions(float: Float.pi)
    static let zero = MyOptions(float: 0.0)
}
