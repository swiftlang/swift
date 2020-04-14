// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

struct Adder {
    var base: Int
    func callAsFunction(_ x: Int) -> Int {
// CHECK: [[@LINE-1]]:10 | instance-method/Swift | callAsFunction(_:) | [[callAsFunc1:.*]] | Def
        return base + x
    }
    func callAsFunction(x: Int, y: Int) -> Adder {
// CHECK: [[@LINE-1]]:10 | instance-method/Swift | callAsFunction(x:y:) | [[callAsFunc2:.*]] | Def
        return base + x + y
    }
}

let add3 = Adder(base: 3)
// CHECK: [[@LINE-1]]:5 | variable/Swift | add3 | [[add3:.*]] | Def
let global = 1

add3(global)
// CHECK: [[@LINE-1]]:1 | variable/Swift | add3 | [[add3]] | Ref,Read |
// CHECK: [[@LINE-2]]:5 | instance-method/Swift | callAsFunction(_:) | [[callAsFunc1]] | Ref,Call,RelRec | rel: 1
// CHECK:   RelRec | struct/Swift | Adder |
// CHECK: [[@LINE-4]]:6 | variable/Swift | global | {{.*}} | Ref,Read |

add3(x: 10, y: 11)
// CHECK: [[@LINE-1]]:1 | variable/Swift | add3 | [[add3]] | Ref,Read |
// CHECK: [[@LINE-2]]:5 | instance-method/Swift | callAsFunction(x:y:) | [[callAsFunc2]] | Ref,Call,RelRec | rel: 1
// CHECK:   RelRec | struct/Swift | Adder |

func getAdder(_ base: Int) -> Adder { return Adder(base: base) }
// CHECK: [[@LINE-1]]:6 | function/Swift | getAdder(_:) | [[getAdder:.*]] | Def | rel: 0

getAdder(5)(10)
// CHECK: [[@LINE-1]]:1 | function/Swift | getAdder(_:) | [[getAdder]] | Ref,Call | rel: 0
// CHECK: [[@LINE-2]]:12 | instance-method/Swift | callAsFunction(_:) | [[callAsFunc1]] | Ref,Call,RelRec | rel: 1
// CHECK:   RelRec | struct/Swift | Adder |

getAdder(5)(x: 1, y: 42)
// CHECK: [[@LINE-1]]:1 | function/Swift | getAdder(_:) | [[getAdder]] | Ref,Call | rel: 0
// CHECK: [[@LINE-2]]:12 | instance-method/Swift | callAsFunction(x:y:) | [[callAsFunc2]] | Ref,Call,RelRec | rel: 1
// CHECK:   RelRec | struct/Swift | Adder |

((add3.callAsFunction)(x: 5, y: 10))(x: 1, y: 42)
// CHECK: [[@LINE-1]]:8 | instance-method/Swift | callAsFunction(x:y:) | [[callAsFunc2]] | Ref,Call,RelRec | rel: 1
// CHECK:   RelRec | struct/Swift | Adder |
// CHECK: [[@LINE-3]]:37 | instance-method/Swift | callAsFunction(x:y:) | [[callAsFunc2]] | Ref,Call,RelRec | rel: 1
// CHECK:   RelRec | struct/Swift | Adder |
