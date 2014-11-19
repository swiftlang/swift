// RUN: %swift %s -parse -verify

// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/17986597
// Similar to 010-circular-protocol-reference.swift, might be same underlying bug.

protocol A {
    typealias A = B // expected-error {{protocol 'B' can only be used as a generic constraint because it has Self or associated type requirements}}
}

protocol B {
    typealias B = A // expected-error {{protocol 'A' can only be used as a generic constraint because it has Self or associated type requirements}}
}
