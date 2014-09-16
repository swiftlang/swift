// RUN: %swift -emit-ir -verify %s
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/17225563

enum a<T> { // expected-error{{recursive value type 'a<T>' is not allowed}}
    case s(T, a)
}
