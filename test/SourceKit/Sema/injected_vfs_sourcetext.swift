func foo(_ structDefinedInSameTarget: StructDefinedInSameTarget) {
    let c: Double = structDefinedInSameTarget.methodDefinedInSameTarget()
// CHECK: cannot convert value of type '()' to specified type 'Double'
}

// RUN: %sourcekitd-test -req=open -vfs-files=/target_file1.swift=@%s,/target_file2.swift=@%S/../Inputs/vfs/other_file_in_target.swift /target_file1.swift -pass-as-sourcetext -- /target_file1.swift /target_file2.swift -target %target-triple == \
// RUN:    -req=print-diags -vfs-files=/target_file1.swift=@%s,/target_file2.swift=@%S/../Inputs/vfs/other_file_in_target.swift /target_file1.swift | %FileCheck %s
