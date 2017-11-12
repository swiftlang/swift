// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=A | %FileCheck %s
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=B | %FileCheck %s
// CHECK: Decl[InstanceMethod]/CurrNominal: f()[#Void#]{{; name=.+$}}
class C1 {
  func f() {
    #^A^#
  deinit {}
}

class C2 {
  func f() {
    guard let x = Optional(1) else {
      #^B^#
  }
  deinit {}
}
