// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=BAD_PROPERTY_1 | FileCheck %s -check-prefix=BAD_PROPERTY_1

class BadProperty1 {
  var prop: Int {
    get {}
    a
  }
}
func badProperty1(a: BadProperty1) {
  a#^BAD_PROPERTY_1^#
}
// BAD_PROPERTY_1: Begin completions, 1 items
// BAD_PROPERTY_1-NEXT: Decl[InstanceVar]/CurrNominal: .prop[#Int#]{{$}}
// BAD_PROPERTY_1-NEXT: End completions

