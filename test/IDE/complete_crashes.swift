// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BAD_MEMBERS_1 | FileCheck %s -check-prefix=BAD_MEMBERS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BAD_MEMBERS_2 | FileCheck %s -check-prefix=BAD_MEMBERS_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLOSURE_CALLED_IN_PLACE_1 | FileCheck %s -check-prefix=WITH_GLOBAL

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

func globalFunc() {}

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LET_COMPUTED | FileCheck %s -check-prefix=WITH_GLOBAL
class C {
  let x : Int { #^LET_COMPUTED^# }
}

// WITH_GLOBAL: Begin completions
// WITH_GLOBAL-DAG: Decl[FreeFunction]/CurrModule:      globalFunc()[#Void#]{{$}}
// WITH_GLOBAL: End completions

({ x in 2+x })(#^CLOSURE_CALLED_IN_PLACE_1^#

