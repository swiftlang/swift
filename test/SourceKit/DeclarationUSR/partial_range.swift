// RUN: %sourcekitd-test -req=collect-decl-usr -req-opts=offset=292 %s -- %s | %FileCheck %s -check-prefix=OFFSET-ONLY
// RUN: %sourcekitd-test -req=collect-decl-usr -req-opts=length=292 %s -- %s | %FileCheck %s -check-prefix=LENGTH-ONLY

struct BeforeRange {
  let beforeMember: Int
}

func afterRange(parameter: Int) -> Int {
  return parameter
}

// OFFSET-ONLY-NOT: BeforeRange
// OFFSET-ONLY-NOT: beforeMember
// OFFSET-ONLY: key.kind: source.lang.swift.decl.function.free
// OFFSET-ONLY-NEXT: key.usr:
// OFFSET-ONLY-SAME: afterRange
// OFFSET-ONLY: key.kind: source.lang.swift.decl.var.parameter
// OFFSET-ONLY-NEXT: key.usr:
// OFFSET-ONLY-SAME: parameter

// LENGTH-ONLY: key.kind: source.lang.swift.decl.struct
// LENGTH-ONLY-NEXT: key.usr:
// LENGTH-ONLY-SAME: BeforeRange
// LENGTH-ONLY: key.kind: source.lang.swift.decl.var.instance
// LENGTH-ONLY-NEXT: key.usr:
// LENGTH-ONLY-SAME: beforeMember
// LENGTH-ONLY-NOT: afterRange
