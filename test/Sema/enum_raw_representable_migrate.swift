// RUN: %target-parse-verify-swift

// Test Fix-Its to migrate fromRaw/toRaw to the failable initializer
// and rawValue property, respectively.

enum Color : Int {
  case Red = 0, Green = 1, Blue = 2
}

let color: Color = Color.fromRaw(1)! // expected-error{{static method 'fromRaw' has been replaced with a failable initializer 'init(rawValue:)'}}{{25-34=(rawValue: }}
let i = color.toRaw() // expected-error{{method 'fromRaw' has been replaced with a property 'rawValue'}}{{15-22=rawValue}}
let i2 = Color.fromRaw(2)!.toRaw()
// expected-error @-1{{static method 'fromRaw' has been replaced with a failable initializer 'init(rawValue:)'}}{{15-24=(rawValue: }}
// expected-error @-2{{method 'fromRaw' has been replaced with a property 'rawValue'}}{{28-35=rawValue}}
