// RUN: %sourcekitd-test -req=cursor -pos=1:10 %S/../Inputs/forbid_typecheck_primary.swift -- \
// RUN:     -Xfrontend -debug-forbid-typecheck-prefix -Xfrontend NOTYPECHECK -module-name forbid \
// RUN:     %S/../Inputs/forbid_typecheck_2.swift %S/../Inputs/forbid_typecheck_primary.swift | %FileCheck -check-prefix=CHECK1 %s

// CHECK1: source.lang.swift.decl.var.global (1:5-1:15)
// CHECK1: globalPrim

// RUN: %sourcekitd-test -req=cursor -pos=5:20 %S/../Inputs/forbid_typecheck_primary.swift -- \
// RUN:     -Xfrontend -debug-forbid-typecheck-prefix -Xfrontend NOTYPECHECK -module-name forbid \
// RUN:     %S/../Inputs/forbid_typecheck_2.swift %S/../Inputs/forbid_typecheck_primary.swift | %FileCheck -check-prefix=CHECK2 %s

// CHECK2: source.lang.swift.ref.var.instance ({{.*}}forbid_typecheck_2.swift:10:7-10:13)
// CHECK2: member
