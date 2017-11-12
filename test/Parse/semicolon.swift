// RUN: %target-typecheck-verify-swift

let a = 42;
var b = "b";

struct A {
    var a1: Int;
    let a2: Int ;
    var a3: Int;let a4: Int
    var a5: Int; let a6: Int;
};

enum B {
    case B1;
    case B2(value: Int);
    case B3
    case B4; case B5 ; case B6;
};

class C {
    var x: Int;
    let y = 3.14159;
    init(x: Int) { self.x = x; }
};

typealias C1 = C;

protocol D {
    var foo: () -> Int { get };
}

struct D1: D {
    let foo = { return 42; };
}
func e() -> Bool {
    return false;
}

import Swift;

for i in 1..<1000 {
    if i % 2 == 1 {
        break;
    };
}

let six = (1..<3).reduce(0, +);

func lessThanTwo(input: UInt) -> Bool {
    switch input {
    case 0:     return true;
    case 1, 2:  return true;
    default:
        return false;
    }
}

enum StarWars {
    enum Quality { case 😀; case 🙂; case 😐; case 😏; case 😞 };
    case Ep4; case Ep5; case Ep6
    case Ep1, Ep2; case Ep3;
};

