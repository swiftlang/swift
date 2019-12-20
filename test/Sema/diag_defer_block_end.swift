// RUN: %target-typecheck-verify-swift

let x = 1
let y = 2
if (x > y) {
    defer { // expected-warning {{'defer' statement at end of scope always executes immediately}}{{5-10=do}}
        print("not so useful defer stmt.")
    }
}

func sr7307(_ value: Bool) {
    let negated = !value 
    defer { // expected-warning {{'defer' statement at end of scope always executes immediately}}{{5-10=do}}
        print("negated value is \(negated)")
    }
}

sr7307(true)

defer { // No note
    print("end of program.")
}

