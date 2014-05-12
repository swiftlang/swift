// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=BAD_MEMBERS_1 | FileCheck %s -check-prefix=BAD_MEMBERS_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=BAD_MEMBERS_2 | FileCheck %s -check-prefix=BAD_MEMBERS_2

class BadMembers1 {
  var prop: Int {
    get {}
    a
  }
  subscript(i: Int) -> Double {
  }
}
func badMembers1(a: BadMembers1) {
  a#^BAD_MEMBERS_1^#
}
// BAD_MEMBERS_1: Begin completions, 2 items
// BAD_MEMBERS_1-NEXT: Decl[InstanceVar]/CurrNominal: .prop[#Int#]{{$}}
// BAD_MEMBERS_1-NEXT: Decl[Subscript]/CurrNominal:   [{#i: Int#}][#Double#]{{$}}
// BAD_MEMBERS_1-NEXT: End completions

protocol BadMembers2 {
  var prop: Int {
    get {}
    a
  }
  subscript(i: Int) -> Double {
  }
}
func badMembers2(a: BadMembers2) {
  a#^BAD_MEMBERS_2^#
}
// BAD_MEMBERS_2: Begin completions, 2 items
// BAD_MEMBERS_2-NEXT: Decl[InstanceVar]/CurrNominal: .prop[#Int#]{{$}}
// BAD_MEMBERS_2-NEXT: Decl[Subscript]/CurrNominal:   [{#i: Int#}][#Double#]{{$}}
// BAD_MEMBERS_2-NEXT: End completions

