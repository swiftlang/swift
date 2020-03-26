func foo(_ structDefinedInSameTarget: StructDefinedInSameTarget) {
    let c: Double = structDefinedInSameTarget.methodDefinedInSameTarget()
// CHECK: cannot convert value of type '()' to specified type 'Double'
}

// RUN: %sourcekitd-test -req=open -vfs-files=%t/VFS/target_file1.swift=@%s,%t/VFS/target_file2.swift=@%S/../Inputs/vfs/other_file_in_target.swift %t/VFS/target_file1.swift -pass-as-sourcetext -- %t/VFS/target_file1.swift %t/VFS/target_file2.swift -target %target-triple == \
// RUN:    -req=print-diags -vfs-files=%t/VFS/target_file1.swift=@%s,%t/VFS/target_file2.swift=@%S/../Inputs/vfs/other_file_in_target.swift %t/VFS/target_file1.swift | %FileCheck %s
