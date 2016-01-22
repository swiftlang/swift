()
// RUN: %sourcekitd-test -req=complete -pos=1:2 %s -- %s | FileCheck %s -check-prefix=KEYWORDS
// RUN: %sourcekitd-test -req=complete.open -pos=1:2 %s -- %s | FileCheck %s -check-prefix=LITERALS

// KEYWORDS-NOT: source.lang.swift.literal
// KEYWORDS: key.name: "nil"
// KEYWORDS-NOT: source.lang.swift.literal
// LITERALS: key.kind: source.lang.swift.literal.boolean
// LITERALS: key.kind: source.lang.swift.literal.string
// LITERALS: key.sourcetext: "\"<#{{.*}}#>\""
// LITERALS: key.kind: source.lang.swift.literal.array
// LITERALS: key.sourcetext: "[<#{{.*}}#>]"
// LITERALS: key.kind: source.lang.swift.literal.dictionary
// LITERALS: key.sourcetext: "[<#{{.*}}#>: <#{{.*}}#>]"
// LITERALS: key.kind: source.lang.swift.literal.tuple
// LITERALS: key.sourcetext: "(<#{{.*}}#>, <#{{.*}}#>)"
// LITERALS: key.kind: source.lang.swift.literal.nil

// RUN: %complete-test -tok=STMT1 %s -raw | FileCheck %s -check-prefix=STMT
// RUN: %complete-test -tok=STMT2 %s -raw | FileCheck %s -check-prefix=STMT
// RUN: %complete-test -tok=STMT3 %s -raw | FileCheck %s -check-prefix=STMT
// STMT-NOT: source.lang.swift.literal

#^STMT1^#

if true {
  #^STMT2^#
}
func foo(x: Int) {
  #^STMT3^#
}

// RUN: %complete-test -tok=EXPR1 %s -raw | FileCheck %s -check-prefix=LITERALS
// RUN: %complete-test -tok=EXPR2 %s -raw | FileCheck %s -check-prefix=LITERALS
// RUN: %complete-test -tok=EXPR3 %s -raw | FileCheck %s -check-prefix=LITERALS
let x = #^EXPR1^#
x + #^EXPR2^#
if #^EXPR3^# { }
foo(#^EXPR3^#)
