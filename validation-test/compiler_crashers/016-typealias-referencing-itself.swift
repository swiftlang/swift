// RUN: %swift %s -parse -verify
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

class a {
    typealias b = b // expected-error {{type alias 'b' circularly references itself}}
}
