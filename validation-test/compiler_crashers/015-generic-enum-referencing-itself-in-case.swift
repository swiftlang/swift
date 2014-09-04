// RUN: not --crash %swift -emit-ir %s
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/17225563

enum a<T> {
    case s(T, a)
}
