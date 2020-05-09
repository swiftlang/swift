// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

class SelfCycle : SelfCycle {}
// CHECK: [[@LINE-1]]:7 | class/Swift | SelfCycle | {{[^ ]*}} | Def | rel: 0
// CHECK: [[@LINE-2]]:19 | class/Swift | SelfCycle | {{[^ ]*}} | Ref,RelBase | rel: 1
// CHECK:   RelBase | class/Swift | SelfCycle | {{\W*}}

class Cycle1_A: Cycle1_B {}
// CHECK: [[@LINE-1]]:7 | class/Swift | Cycle1_A | {{[^ ]*}} | Def | rel: 0
// CHECK: [[@LINE-2]]:17 | class/Swift | Cycle1_B | {{[^ ]*}} | Ref,RelBase | rel: 1
// CHECK:   RelBase | class/Swift | Cycle1_A | {{[^ ]*}}
class Cycle1_B: Cycle1_A {}
// CHECK: [[@LINE-1]]:7 | class/Swift | Cycle1_B | {{[^ ]*}} | Def | rel: 0
// CHECK: [[@LINE-2]]:17 | class/Swift | Cycle1_A | {{[^ ]*}} | Ref,RelBase | rel: 1
// CHECK:   RelBase | class/Swift | Cycle1_B | {{[^ ]*}}

class Cycle2_A: Cycle2_C {}
// CHECK: [[@LINE-1]]:7 | class/Swift | Cycle2_A | {{[^ ]*}} | Def | rel: 0
// CHECK: [[@LINE-2]]:17 | class/Swift | Cycle2_C | {{[^ ]*}} | Ref,RelBase | rel: 1
// CHECK:   RelBase | class/Swift | Cycle2_A | {{[^ ]*}}
class Cycle2_B: Cycle2_A {}
// CHECK: [[@LINE-1]]:7 | class/Swift | Cycle2_B | {{[^ ]*}} | Def | rel: 0
// CHECK: [[@LINE-2]]:17 | class/Swift | Cycle2_A | {{[^ ]*}} | Ref,RelBase | rel: 1
// CHECK:   RelBase | class/Swift | Cycle2_B | {{[^ ]*}}
class Cycle2_C: Cycle2_B {}
// CHECK: [[@LINE-1]]:7 | class/Swift | Cycle2_C | {{[^ ]*}} | Def | rel: 0
// CHECK: [[@LINE-2]]:17 | class/Swift | Cycle2_B | {{[^ ]*}} | Ref,RelBase | rel: 1
// CHECK:   RelBase | class/Swift | Cycle2_C | {{[^ ]*}}

class TestCase1: XCTestCase {}
// CHECK: [[@LINE-1]]:7 | class(test)/Swift | TestCase1 | {{[^ ]*}} | Def | rel: 0
// CHECK: [[@LINE-2]]:18 | class/Swift | XCTestCase | {{[^ ]*}} | Ref,RelBase | rel: 1
// CHECK:   RelBase | class(test)/Swift | TestCase1 | {{[^ ]*}}
class XCTestCase: TestCase1 {}
// CHECK: [[@LINE-1]]:7 | class/Swift | XCTestCase | {{[^ ]*}} | Def | rel: 0
// CHECK: [[@LINE-2]]:19 | class(test)/Swift | TestCase1 | {{[^ ]*}} | Ref,RelBase | rel: 1
// CHECK:   RelBase | class/Swift | XCTestCase | {{[^ ]*}}
class TestCase2: TestCase1 {}
// CHECK: [[@LINE-1]]:7 | class(test)/Swift | TestCase2 | {{[^ ]*}} | Def | rel: 0
// CHECK: [[@LINE-2]]:18 | class(test)/Swift | TestCase1 | {{[^ ]*}} | Ref,RelBase | rel: 1
// CHECK:   RelBase | class(test)/Swift | TestCase2 | {{[^ ]*}}

protocol SelfCycleP: SelfCycleP {}
// CHECK: [[@LINE-1]]:10 | protocol/Swift | SelfCycleP | {{[^ ]*}} | Def | rel: 0
// CHECK: [[@LINE-2]]:22 | protocol/Swift | SelfCycleP | {{[^ ]*}} | Ref,RelBase | rel: 1
// CHECK:   RelBase | protocol/Swift | SelfCycleP | {{[^ ]*}}
protocol Cycle1P_A: Cycle1P_B {}
// CHECK: [[@LINE-1]]:10 | protocol/Swift | Cycle1P_A | {{[^ ]*}} | Def | rel: 0
// CHECK: [[@LINE-2]]:21 | protocol/Swift | Cycle1P_B | {{[^ ]*}} | Ref,RelBase | rel: 1
// CHECK:   RelBase | protocol/Swift | Cycle1P_A | {{[^ ]*}}
protocol Cycle1P_B: Cycle1P_A {}
// CHECK: [[@LINE-1]]:10 | protocol/Swift | Cycle1P_B | {{[^ ]*}} | Def | rel: 0
// CHECK: [[@LINE-2]]:21 | protocol/Swift | Cycle1P_A | {{[^ ]*}} | Ref,RelBase | rel: 1
// CHECK:   RelBase | protocol/Swift | Cycle1P_B | {{[^ ]*}}
protocol Cycle2P_A: Cycle2P_C {}
// CHECK: [[@LINE-1]]:10 | protocol/Swift | Cycle2P_A | {{[^ ]*}} | Def | rel: 0
// CHECK: [[@LINE-2]]:21 | protocol/Swift | Cycle2P_C | {{[^ ]*}} | Ref,RelBase | rel: 1
// CHECK:   RelBase | protocol/Swift | Cycle2P_A | {{[^ ]*}}
protocol Cycle2P_B: Cycle2P_A {}
// CHECK: [[@LINE-1]]:10 | protocol/Swift | Cycle2P_B | {{[^ ]*}} | Def | rel: 0
// CHECK: [[@LINE-2]]:21 | protocol/Swift | Cycle2P_A | {{[^ ]*}} | Ref,RelBase | rel: 1
// CHECK:   RelBase | protocol/Swift | Cycle2P_B | {{[^ ]*}}
protocol Cycle2P_C: Cycle2P_B {}
// CHECK: [[@LINE-1]]:10 | protocol/Swift | Cycle2P_C | {{[^ ]*}} | Def | rel: 0
// CHECK: [[@LINE-2]]:21 | protocol/Swift | Cycle2P_B | {{[^ ]*}} | Ref,RelBase | rel: 1
// CHECK:   RelBase | protocol/Swift | Cycle2P_C | {{[^ ]*}}
