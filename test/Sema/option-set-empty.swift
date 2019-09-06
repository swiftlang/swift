struct MyOptions: OptionSet {
    var rawValue: Int
    static let none = MyOptions(rawValue: 0) // expected-warning {{'static let' constant inside 'MyOptions' that conforms to OptionSet produces an empty option set}}
}
