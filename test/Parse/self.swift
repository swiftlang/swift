let _ = self // expected-error {{Cannot find `self` in scope, did you mean to declare or extend a type?}} 
let s = self.b // // expected-error {{Cannot find `self` in scope, did you mean to declare or extend a type?}} 
let w = self.method() // expected-error {{Cannot find `self` in scope, did you mean to declare or extend a type?}} 

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
