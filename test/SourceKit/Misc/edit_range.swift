// RUN: %sourcekitd-test \
// RUN:   -req=open %S/Inputs/10bytes.swift -- %S/Inputs/10bytes.swift == \
// RUN:   -req=edit -offset=10 -length=0 -replace="swift" %S/Inputs/10bytes.swift -- %S/Inputs/10bytes.swift

// RUN: %sourcekitd-test \
// RUN:   -req=open %S/Inputs/10bytes.swift -- %S/Inputs/10bytes.swift == \
// RUN:   -req=edit -offset=5 -length=5 -replace="swift" %S/Inputs/10bytes.swift -- %S/Inputs/10bytes.swift

// 'offset' out of range.
// RUN: not %sourcekitd-test \
// RUN:   -req=open %S/Inputs/10bytes.swift -- %S/Inputs/10bytes.swift == \
// RUN:   -req=edit -offset=11 -length=0 -replace="swift" %S/Inputs/10bytes.swift -- %S/Inputs/10bytes.swift \
// RUN:   2>&1 | %FileCheck %s

// 'offset' + 'length' out of range.
// RUN: not %sourcekitd-test \
// RUN:   -req=open %S/Inputs/10bytes.swift -- %S/Inputs/10bytes.swift == \
// RUN:   -req=edit -offset=5 -length=6 -replace="swift" %S/Inputs/10bytes.swift -- %S/Inputs/10bytes.swift \
// RUN:   2>&1 | %FileCheck %s

// Out of range after edits.
// RUN: not %sourcekitd-test \
// RUN:   -req=open %S/Inputs/10bytes.swift -- %S/Inputs/10bytes.swift == \
// RUN:   -req=edit -offset=5 -length=5 -replace="" %S/Inputs/10bytes.swift -- %S/Inputs/10bytes.swift == \
// RUN:   -req=edit -offset=6 -length=0 -replace="swift" %S/Inputs/10bytes.swift -- %S/Inputs/10bytes.swift \
// RUN:   2>&1 | %FileCheck %s

// CHECK: 'offset' + 'length' is out of range
