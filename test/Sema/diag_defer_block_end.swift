// RUN: %target-typecheck-verify-swift

let x = 1
let y = 2
if (x > y) {
    defer { // expected-warning {{'defer' statement at end of scope always executes immediately}}{{5-10=do}}
        print("not so useful defer stmt.")
    }
}

// https://github.com/apple/swift/issues/49855
do {
  func f(_ value: Bool) {
      let negated = !value
      defer { // expected-warning {{'defer' statement at end of scope always executes immediately}}{{7-12=do}}
          print("negated value is \(negated)")
      }
  }

  f(true)
}

defer { // No note
    print("end of program.")
}

