// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=COMPLETE | FileCheck %s

typealias TestAl = (Int, Int) -> Bool

class Bar {
    func foo1(callback: (Int, Int) -> Void) {
    }
    func foo2(callback: TestAl) {
    }
}

Bar().#^COMPLETE^#


// CHECK: Begin completions, 2 items
// CHECK-DAG: Decl[InstanceMethod]/CurrNominal:   foo1({#(callback): (Int, Int) -> Void##(Int, Int) -> Void#})[#Void#]
// CHECK-DAG: Decl[InstanceMethod]/CurrNominal:   foo2({#(callback): TestAl##(Int, Int) -> Bool#})[#Void#]
// CHECK: End completions
