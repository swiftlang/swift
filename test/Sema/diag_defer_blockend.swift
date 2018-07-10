// RUN: %target-swift-frontend -typecheck -verify %s

let x = 1
let y = 2
if (x > y) {
    defer { // expected-note {{defer statement at the end of block is pointless}}
        print("not so useful defer stmt.")
    }
}

func sr7307(_ value: Bool) {
    let negated = !value 
    defer { // expected-note {{defer statement at the end of block is pointless}}
        print("negated value is {negated}")
    }
}

sr7307(true)

defer { // No note
    print("end of program.")
}

