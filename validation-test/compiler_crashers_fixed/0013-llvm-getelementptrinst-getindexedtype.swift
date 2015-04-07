// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/17359448

protocol a {
    static func c()
}
class b: a {
    class func c() { }
}
(b() as a).dynamicType.c() // expected-error {{accessing members of protocol type value '(a).Type' is unimplemented}}
