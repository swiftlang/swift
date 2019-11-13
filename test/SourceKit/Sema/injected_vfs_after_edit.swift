func foo(_ structDefinedInSameTarget: StructDefinedInSameTarget) {
    let _: Double = structDefinedInSameTarget.methodDefinedInSameTarget()
// CHECK: cannot convert value of type '()' to specified type 'Double'
// CHECK: cannot convert value of type '()' to specified type 'Int'
}

// RUN: %sourcekitd-test -req=open -vfs-files=/target_file2.swift=%S/../Inputs/vfs/other_file_in_target.swift %s -pass-as-sourcetext -- %s /target_file2.swift -target %target-triple == \
// RUN:    -req=print-diags %s == \
// RUN:    -req=edit %s -pos=2:12 -length=6 -replace='Int' == \
// RUN:    -req=print-diags %s | %FileCheck %s
