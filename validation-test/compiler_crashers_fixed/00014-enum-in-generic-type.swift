// RUN: %target-swift-frontend %s -parse -verify

// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/17330553

class a<T> {
    enum b { // expected-error {{type 'b' nested in generic type 'a' is not allowed}}
        case c
    }
}
