// RUN: %target-parse-verify-swift

typealias rgb = Int32 // expected-note {{declared here}}
var rgb : rgb? // expected-error {{invalid redeclaration of 'rgb'}}


struct Color {
    var rgba : rgba? { // expected-error {{'rgba' used within its own type}}
        return nil
    }

    typealias rgba = Int32
}

struct Color2 {
    let rgba : rgba? // expected-error {{'rgba' used within its own type}}

    struct rgba {}
}
