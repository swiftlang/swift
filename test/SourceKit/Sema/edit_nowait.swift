// RUN: %empty-directory(%t)
// RUN: echo "" > %t/t.swift

// RUN: %sourcekitd-test \
// RUN:   -req=open %t/t.swift -- %t/t.swift == \
// RUN:   -req=edit -offset=0 -replace="func foo() { warn("") }" -length=0 -req-opts=enablesyntaxmap=0,enablesubstructure=0,enablediagnostics=0 %t/t.swift -print-raw-response == \
// RUN:   -req=edit -offset=13 -replace="print" -length=5 -req-opts=enablesyntaxmap=0,enablesubstructure=0,enablediagnostics=0 %t/t.swift -print-raw-response \
// RUN: | %FileCheck --check-prefix=EDIT_NOWAIT %s

// EDIT_NOWAIT:      {
// EDIT_NOWAIT-NEXT: }
// EDIT_NOWAIT-NEXT: {
// EDIT_NOWAIT-NEXT: }

// RUN: %sourcekitd-test \
// RUN: %sourcekitd-test \
// RUN:   -req=open %t/t.swift -- %t/t.swift == \
// RUN:   -req=edit -offset=0 -replace="func foo() { warn("") }" -length=0 -req-opts=enablesyntaxmap=0,enablesubstructure=0,enablediagnostics=0 %t/t.swift == \
// RUN:   -req=edit -offset=13 -replace="print" -length=4 -req-opts=enablesyntaxmap=0,enablesubstructure=0,enablediagnostics=0 %t/t.swift == \
// RUN:   -req=print-annotations %s \
// RUN: | %FileCheck --check-prefix=ANNOTATION %s

// ANNOTATION:      [
// ANNOTATION-NEXT:   {
// ANNOTATION-NEXT:     key.kind: source.lang.swift.ref.function.free,
// ANNOTATION-NEXT:     key.offset: 13,
// ANNOTATION-NEXT:     key.length: 5,
// ANNOTATION-NEXT:     key.is_system: 1
// ANNOTATION-NEXT:   }
// ANNOTATION-NEXT: ]
