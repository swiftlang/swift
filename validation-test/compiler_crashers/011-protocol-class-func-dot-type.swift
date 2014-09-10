// RUN: %swift %s -parse -verify
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

protocol b {
    class func e()
}

struct c {
    var d: b.Type
    func e() {
        d.e() // expected-error {{accessing members of protocol type value 'b.Type' is unimplemented}}
    }
}
