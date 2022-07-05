// RUN: %target-typecheck-verify-swift

enum  Colors {
	case blue
	case red
}

let globalString = ""
let globalInt = 2
let globalColor = Colors.blue

func giveMeBlue() -> Colors {
	return .blue
}

@propertyWrapper struct Wrapper<Value> {
  let key: String
  var wrappedValue: Value? { get { return nil } set { } }
  init(_ key: _const String) {self.key = key }
  init(_ key: _const Int) {self.key = "" }
  init(_ key: _const Colors) { self.key = ""}
}

struct WrapperAdopters {
    @Wrapper<Bool>(3)
    var wrappedVar_correct_1

    @Wrapper<Bool>("")
    var wrappedVar_correct_2

    @Wrapper<Bool>(.blue)
    var wrappedVar_correct_3

    @Wrapper<Bool>(Colors.blue)
    var wrappedVar_correct_4
}

struct WrapperAdopters_incorrect {
    @Wrapper<Bool>(globalInt) // expected-error{{expect a compile-time constant literal}}
    var wrappedVar_incorrect_1

    @Wrapper<Bool>(globalString) // expected-error{{expect a compile-time constant literal}}
    var wrappedVar_incorrect_2

    @Wrapper<Bool>(globalColor) // expected-error{{expect a compile-time constant literal}}
    var wrappedVar_incorrect_3

    @Wrapper<Bool>(giveMeBlue()) // expected-error{{expect a compile-time constant literal}}
    var wrappedVar_incorrect_4
}
