// RUN: %target-parse-verify-swift
struct X<T> { // expected-error {{recursive value type 'X<T>' is not allowed}}
    let s: X<X>
}

enum Y<T> { // expected-error {{recursive enum 'Y<T>' is not marked 'indirect'}}
    case S(Y<Y>)
}
