import swift_mod_syn

func f(s : inout [Int]) {
  s.sort()
}

// RUN: %empty-directory(%t.mod)
// RUN: %swift -emit-module -o %t.mod/swift_mod.swiftmodule %S/Inputs/swift_mod.swift -parse-as-library
// RUN: %sourcekitd-test -req=interface-gen -module swift_mod -- -I %t.mod > %t.response
// RUN: diff -u %s.response %t.response

// RUN: %sourcekitd-test -req=module-groups -module swift_mod -- -I %t.mod | %FileCheck -check-prefix=GROUP-EMPTY %s
// GROUP-EMPTY: <GROUPS>
// GROUP-EMPTY-NEXT: <\GROUPS>

// RUN: %swift -emit-module -o %t.mod/swift_mod_syn.swiftmodule %S/Inputs/swift_mod_syn.swift -parse-as-library
// RUN: %sourcekitd-test -req=interface-gen-open -module swift_mod_syn -- -I %t.mod == -req=cursor -pos=4:7 %s -- %s -I %t.mod | %FileCheck -check-prefix=SYNTHESIZED-USR1 %s
// SYNTHESIZED-USR1: s:SMsSkRzSL7ElementSTRpzrlE4sortyyF::SYNTHESIZED::s:Sa

// RUN: %sourcekitd-test -req=interface-gen-open -module Swift -synthesized-extension \
// RUN: 	== -req=find-usr -usr "s:SMsSkRzSL7ElementSTRpzrlE4sortyyF::SYNTHESIZED::s:Sa" | %FileCheck -check-prefix=SYNTHESIZED-USR2 %s
// SYNTHESIZED-USR2-NOT: USR NOT FOUND

// RUN: %sourcekitd-test -req=interface-gen-open -module Swift \
// RUN: 	== -req=find-usr -usr "s:SMsSkRzSL7ElementSTRpzrlE4sortyyF::SYNTHESIZED::s:Sa::SYNTHESIZED::USRDOESNOTEXIST" | %FileCheck -check-prefix=SYNTHESIZED-USR3 %s
// SYNTHESIZED-USR3-NOT: USR NOT FOUND
