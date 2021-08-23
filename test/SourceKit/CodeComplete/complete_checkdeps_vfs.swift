func foo(value: MyStruct) {
  value./*HERE*/
}

// REQUIRES: shell

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/VFS)
// RUN: cp %S/Inputs/checkdeps/MyProject/LibraryExt.swift %t/VFS/

// RUN: %sourcekitd-test \
// RUN:   -req=global-config -req-opts=completion_check_dependency_interval=100 == \

// RUN:   -shell -- echo "### Initial" == \
// RUN:   -req=complete -pos=2:9 -pass-as-sourcetext -vfs-files=%t/VFS/Main.swift=@%s,%t/VFS/Library.swift=@%S/Inputs/checkdeps/MyProject/Library.swift %t/VFS/Main.swift -- -target %target-triple %t/VFS/Main.swift %t/VFS/LibraryExt.swift %t/VFS/Library.swift == \

// RUN:   -shell -- echo "### Modify" == \
// RUN:   -req=global-config -req-opts=completion_check_dependency_interval=0 == \
// RUN:   -req=complete -pos=2:9 -pass-as-sourcetext -vfs-files=%t/VFS/Main.swift=@%s,%t/VFS/Library.swift=@%S/Inputs/checkdeps/MyProject_mod/Library.swift %t/VFS/Main.swift -- -target %target-triple %t/VFS/Main.swift %t/VFS/LibraryExt.swift %t/VFS/Library.swift == \

// RUN:   -shell -- echo "### Keep" == \
// RUN:   -req=global-config -req-opts=completion_check_dependency_interval=0 == \
// RUN:   -req=complete -pos=2:9 -pass-as-sourcetext -vfs-files=%t/VFS/Main.swift=@%s,%t/VFS/Library.swift=@%S/Inputs/checkdeps/MyProject_mod/Library.swift %t/VFS/Main.swift -- -target %target-triple %t/VFS/Main.swift %t/VFS/LibraryExt.swift %t/VFS/Library.swift == \

// RUN:   -shell -- echo "### Rollback without check" == \
// RUN:   -req=global-config -req-opts=completion_check_dependency_interval=100 == \
// RUN:   -req=complete -pos=2:9 -pass-as-sourcetext -vfs-files=%t/VFS/Main.swift=@%s,%t/VFS/Library.swift=@%S/Inputs/checkdeps/MyProject/Library.swift %t/VFS/Main.swift -- -target %target-triple %t/VFS/Main.swift %t/VFS/LibraryExt.swift %t/VFS/Library.swift == \

// RUN:   -shell -- echo "### Check" == \
// RUN:   -req=global-config -req-opts=completion_check_dependency_interval=0 == \
// RUN:   -req=complete -pos=2:9 -pass-as-sourcetext -vfs-files=%t/VFS/Main.swift=@%s,%t/VFS/Library.swift=@%S/Inputs/checkdeps/MyProject/Library.swift %t/VFS/Main.swift -- -target %target-triple %t/VFS/Main.swift %t/VFS/LibraryExt.swift %t/VFS/Library.swift \

// RUN:   |  %FileCheck %s

// CHECK-LABEL: ### Initial
// CHECK: key.results: [
// CHECK-DAG: key.description: "myStructMethod()"
// CHECK-DAG: key.description: "extensionMethod()"
// CHECK-DAG: key.description: "self"
// CHECK: ]
// CHECK-NOT: key.reusingastcontext: 1

// CHECK-LABEL: ### Modify
// CHECK: key.results: [
// CHECK-DAG: key.description: "myStructMethod_mod()"
// CHECK-DAG: key.description: "extensionMethod()"
// CHECK-DAG: key.description: "self"
// CHECK: ]
// CHECK-NOT: key.reusingastcontext: 1

// CHECK-LABEL: ### Keep
// CHECK: key.results: [
// CHECK-DAG: key.description: "myStructMethod_mod()"
// CHECK-DAG: key.description: "extensionMethod()"
// CHECK-DAG: key.description: "self"
// CHECK: ]
// CHECK: key.reusingastcontext: 1

// CHECK-LABEL: ### Rollback without check
// CHECK: key.results: [
// CHECK-DAG: key.description: "myStructMethod_mod()"
// CHECK-DAG: key.description: "extensionMethod()"
// CHECK-DAG: key.description: "self"
// CHECK: ]
// CHECK: key.reusingastcontext: 1

// CHECK-LABEL: ### Check
// CHECK: key.results: [
// CHECK-DAG: key.description: "myStructMethod()"
// CHECK-DAG: key.description: "extensionMethod()"
// CHECK-DAG: key.description: "self"
// CHECK: ]
// CHECK-NOT: key.reusingastcontext: 1
