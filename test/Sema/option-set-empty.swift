// RUN: %target-typecheck-verify-swift

struct MyOptions: OptionSet {
    var rawValue: Int
    static let none = MyOptions(rawValue: 0) // expected-warning {{static property 'none' produces an empty option set}} expected-note {{use [] to silence this warning}}{{32-45=([])}}
}
