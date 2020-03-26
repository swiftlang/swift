// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNINFERRED | %FileCheck %s -check-prefix=UNINFERRED

struct S1<V0> {}
protocol P1 {
    associatedtype A1
}
extension P1 where A1 == S1<Int> {
    subscript<T>(v0: T) -> Int  { fatalError() }
    subscript<T>(v0: T) -> Undefined { fatalError() }
}
struct S2<T> : P1 {
    typealias A1 = S1<T>
}
_ = S2()#^UNINFERRED^#

// UNINFERRED: Decl[Subscript]/Super:     [{#(v0): T#}][#Int#]; name=[v0: T]
// UNINFERRED: Decl[Subscript]/Super:     [{#(v0): T#}][#<<error type>>#]; name=[v0: T]
// UNINFERRED: Keyword[self]/CurrNominal: .self[#S2<_>#]; name=self
