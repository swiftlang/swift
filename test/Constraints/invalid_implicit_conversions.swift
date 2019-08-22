// RUN: %target-typecheck-verify-swift

func takesAutoclosure<T>(_ lhs: T, _ rhs: @autoclosure () throws -> T) {}

func test(
  _ rawPtr: UnsafeRawPointer,
  _ mutRawPtr: UnsafeMutableRawPointer,
  _ mutPtr: UnsafeMutablePointer<Int>,
  _ ptr: UnsafePointer<Int>,
  _ ptrI8: UnsafePointer<Int8>,
  _ ptrU8: UnsafePointer<UInt8>,
  _ ptrVoid: UnsafePointer<Void>, // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ optPtr: UnsafePointer<Int>?
) {
  var i: Int = 0
  var a: [Int] = [0]
  var b: [Int]? = [0]
  let s = "string"

  takesAutoclosure(rawPtr, &i) // expected-error {{cannot perform pointer conversion of value of type 'Int' to autoclosure result type 'UnsafeRawPointer'}}
  takesAutoclosure(mutRawPtr, &i) // expected-error {{cannot perform pointer conversion of value of type 'Int' to autoclosure result type 'UnsafeMutableRawPointer'}}
  takesAutoclosure(mutPtr, &i) // expected-error {{cannot perform pointer conversion of value of type 'Int' to autoclosure result type 'UnsafeMutablePointer<Int>'}}
  takesAutoclosure(ptr, &i) // expected-error {{cannot perform pointer conversion of value of type 'Int' to autoclosure result type 'UnsafePointer<Int>'}}
  takesAutoclosure(rawPtr, &a) // expected-error {{cannot perform pointer conversion of value of type '[Int]' to autoclosure result type 'UnsafeRawPointer'}}
  takesAutoclosure(mutRawPtr, &a) // expected-error {{cannot perform pointer conversion of value of type '[Int]' to autoclosure result type 'UnsafeMutableRawPointer'}}
  takesAutoclosure(mutPtr, &a) // expected-error {{cannot perform pointer conversion of value of type '[Int]' to autoclosure result type 'UnsafeMutablePointer<Int>'}}
  takesAutoclosure(ptr, &a) // expected-error {{cannot perform pointer conversion of value of type '[Int]' to autoclosure result type 'UnsafePointer<Int>'}}
  takesAutoclosure(optPtr, &i) // expected-error {{cannot perform pointer conversion of value of type 'Int' to autoclosure result type 'UnsafePointer<Int>?'}}

  takesAutoclosure(rawPtr, a) // expected-error {{cannot perform pointer conversion of value of type '[Int]' to autoclosure result type 'UnsafeRawPointer'}}
  takesAutoclosure(ptr, a) // expected-error {{cannot perform pointer conversion of value of type '[Int]' to autoclosure result type 'UnsafePointer<Int>'}}
  takesAutoclosure(optPtr, b) // expected-error {{cannot perform pointer conversion of value of type '[Int]?' to autoclosure result type 'UnsafePointer<Int>?'}}

  takesAutoclosure(rawPtr, s) // expected-error {{cannot perform pointer conversion of value of type 'String' to autoclosure result type 'UnsafeRawPointer'}}
  takesAutoclosure(ptrI8, s) // expected-error {{cannot perform pointer conversion of value of type 'String' to autoclosure result type 'UnsafePointer<Int8>'}}
  takesAutoclosure(ptrU8, s) // expected-error {{cannot perform pointer conversion of value of type 'String' to autoclosure result type 'UnsafePointer<UInt8>'}}
  takesAutoclosure(ptrVoid, s) // expected-error {{cannot perform pointer conversion of value of type 'String' to autoclosure result type 'UnsafePointer<Void>'}}
}
