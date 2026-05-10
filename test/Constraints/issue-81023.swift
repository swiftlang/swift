// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/81023

protocol MyPublisher {
  associatedtype Output
  associatedtype Failure: Error
  func eraseToAnyPublisher() -> MyAnyPublisher<Output, Failure>
}

extension MyPublisher {
  func eraseToAnyPublisher() -> MyAnyPublisher<Output, Failure> {
    fatalError()
  }
}

struct MyAnyPublisher<Output, Failure: Error>: MyPublisher {}
struct MyJust<Output>: MyPublisher {
  typealias Failure = Never
  init(_ value: Output) {}
}

extension MyPublisher where Output == (any Collection)? { // expected-note {{where 'Self.Output' = '[Int]?'}}
  func mapCount() -> MyAnyPublisher<Int, Failure> { fatalError() }
}

func test(myPub: MyAnyPublisher<[Int]?, Never>) {
  myPub.mapCount()
  // expected-error@-1 {{referencing instance method 'mapCount()' on 'MyPublisher' requires the types '[Int]?' and '(any Collection)?' be equivalent}}
}
