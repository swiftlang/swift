// RUN: %swift %s -parse -verify
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

protocol b {
    class func e()
}

struct c {
    var d: b.Type
    func e() {
        d.e() // expected-error {{cannot convert existential metatype 'b.Type' to metatype 'b.Protocol'}}
    }
}
