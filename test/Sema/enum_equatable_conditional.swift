// RUN: %target-typecheck-verify-swift -swift-version 4

struct NotEquatable { }

enum WithArrayOfNotEquatables : Equatable { // expected-error{{type 'WithArrayOfNotEquatables' does not conform to protocol 'Equatable'}} expected-note {{add stubs for conformance}}
  case only([NotEquatable]) // expected-note{{associated value type '[NotEquatable]' does not conform to protocol 'Equatable', preventing synthesized conformance of 'WithArrayOfNotEquatables' to 'Equatable'}}
}

enum WithArrayOfNotEquatables2<T> : Equatable { // expected-error{{type 'WithArrayOfNotEquatables2<T>' does not conform to protocol 'Equatable'}} expected-note {{add stubs for conformance}}
  case only([T]) // expected-note{{associated value type '[T]' does not conform to protocol 'Equatable', preventing synthesized conformance of 'WithArrayOfNotEquatables2<T>' to 'Equatable'}}
}


// Okay: T is Equatable
enum WithArrayOfEquatables1<T: Equatable> : Equatable {
case only([T])
}

enum WithArrayOfEquatables2<T> {
case only([T])
}

// Okay: T is Equatable here too
extension WithArrayOfEquatables2: Equatable where T: Equatable { }

