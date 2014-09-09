// RUN: %swift %s -parse -verify
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/17317691

func f() {
    ({}) // expected-error{{type of expression is ambiguous without more context}}
}
