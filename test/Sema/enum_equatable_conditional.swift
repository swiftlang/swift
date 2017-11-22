// RUN: %target-typecheck-verify-swift

// FIXME: Should be part of the standard library.
extension Array: Equatable where Element: Equatable { }

struct NotEquatable { }

enum WithArrayOfNotEquatables : Equatable { // expected-error{{type 'WithArrayOfNotEquatables' does not conform to protocol 'Equatable'}}
case only([NotEquatable])
}

enum WithArrayOfNotEquatables2<T> : Equatable { // expected-error{{type 'WithArrayOfNotEquatables2<T>' does not conform to protocol 'Equatable'}}
case only([T])
}


// Okay: T is Equatable
enum WithArrayOfEquatables1<T: Equatable> : Equatable {
case only([T])
}

enum WithArrayOfEquatables2<T> {
case only([T])
}

// No: T is Equatable here, but cannot synthesize via an extension.
// expected-error@+1{{type 'WithArrayOfEquatables2<T>' does not conform to protocol 'Equatable'}}
extension WithArrayOfEquatables2: Equatable where T: Equatable { }

