// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

struct BoxedError<T: Error> {
  var contents: T
}

enum MyError: Error {
  case nothingImportant
  case reallyImportant(String)
}

// CHECK: start
print("start")

let value = MyError.reallyImportant("hello")

// CHECK-NEXT: reallyImportant("hello")
print(value)

func takeBoxedError<T>(error: BoxedError<T>) -> Error {
  return error.contents
}

func makeBoxedError<T: Error>(error: T) -> BoxedError<Error> {
  return BoxedError(contents: error)
}

let unboxedValue = takeBoxedError(error: makeBoxedError(error: value))

// CHECK-NEXT: reallyImportant("hello")
print(unboxedValue)

let errorValue: Error = MyError.reallyImportant("goodbye")

func castValueToError<T>(error: T) -> Error? {
  return error as? Error
}

// CHECK-NEXT: reallyImportant("goodbye")
print(castValueToError(error: errorValue) ?? value)

struct Carrier<T> {
  var name: String
}
protocol ErrorCarrier {}
extension Carrier: ErrorCarrier where T: Error {}

func castValueToErrorCarrier<T>(_ value: T) -> ErrorCarrier? {
  return value as? ErrorCarrier
}

// CHECK-NEXT: nil
print(castValueToErrorCarrier(Carrier<Int>(name: "A carrier of numbers")))

// CHECK-NEXT: A carrier of my errors
print(castValueToErrorCarrier(Carrier<MyError>(name: "A carrier of my errors")))

// CHECK-NEXT: A carrier of all errors
print(castValueToErrorCarrier(Carrier<Error>(name: "A carrier of all errors")))

// CHECK-NEXT: nil
protocol ErrorRefinement : Error {}
print(castValueToErrorCarrier(Carrier<ErrorRefinement>(name: "A carrier of refined errors")))

// CHECK-NEXT: nil
protocol OtherProtocol {}
print(castValueToErrorCarrier(Carrier<Error & OtherProtocol>(name: "A carrier of composed errors")))

// CHECK-NEXT: nil
class C {}
print(castValueToErrorCarrier(Carrier<Error & C>(name: "A carrier of classic composed errors")))

// CHECK-NEXT: end
print("end")
