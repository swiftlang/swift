// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s | %FileCheck %s
protocol P {
  associatedtype T: P
}
struct S<A,B where B: P>: P {
  typealias T = B.T
  func f(_: T) -> T {}
  func f(_: B.T) -> T {}
}
func &&&(x: S, y: S) -> Bool {}

S()#^A^#
// CHECK: Decl[InstanceMethod]/CurrNominal:   .f
