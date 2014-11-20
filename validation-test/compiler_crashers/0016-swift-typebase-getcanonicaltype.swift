// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/18405200

class a {
    typealias b = b // expected-error {{type alias 'b' circularly references itself}}
}
