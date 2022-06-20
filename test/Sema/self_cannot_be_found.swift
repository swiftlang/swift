// RUN: %target-typecheck-verify-swift

let _ = self // expected-error {{cannot find 'self' in scope; did you mean to use it in a type or extension context?}} 
let s = self.b // // expected-error {{cannot find 'self' in scope; did you mean to use it in a type or extension context?}} 
let w = self.method() // expected-error {{cannot find 'self' in scope; did you mean to use it in a type or extension context?}} 

struct aStruct {
    let b = "a"

    func testFunc() {
        print(self.b)
    }

    var c: String {
        self.b
    }
}

let _ = aStruct.self
let initialized = aStruct().self
_ = initialized.b
