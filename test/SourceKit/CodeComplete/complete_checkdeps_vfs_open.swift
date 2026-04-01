func foo(value: MyStruct) {
  value./*HERE*/
}


// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t%{fs-sep}VFS)
// RUN: cp %S/Inputs/checkdeps/MyProject/LibraryExt.swift %t%{fs-sep}VFS%{fs-sep}

// RUN: %sourcekitd-test \
// RUN:   -req=global-config -req-opts=completion_check_dependency_interval=100 == \

// RUN:   -shell -- echo "### Initial" == \
// RUN:   -req=complete.open -pos=2:9 -pass-as-sourcetext -vfs-files=%t%{fs-sep}VFS%{fs-sep}Main.swift=@%s,%t%{fs-sep}VFS%{fs-sep}Library.swift=@%S/Inputs/checkdeps/MyProject/Library.swift %t%{fs-sep}VFS%{fs-sep}Main.swift -- -target %target-triple %t%{fs-sep}VFS%{fs-sep}Main.swift %t%{fs-sep}VFS%{fs-sep}LibraryExt.swift %t%{fs-sep}VFS%{fs-sep}Library.swift == \
// RUN:   -req=complete.close -pos=2:9 -name %t%{fs-sep}VFS%{fs-sep}Main.swift %s == \

// RUN:   -shell -- echo "### Modify" == \
// RUN:   -req=global-config -req-opts=completion_check_dependency_interval=0 == \
// RUN:   -req=complete.open -pos=2:9 -pass-as-sourcetext -vfs-files=%t%{fs-sep}VFS%{fs-sep}Main.swift=@%s,%t%{fs-sep}VFS%{fs-sep}Library.swift=@%S/Inputs/checkdeps/MyProject_mod/Library.swift %t%{fs-sep}VFS%{fs-sep}Main.swift -- -target %target-triple %t%{fs-sep}VFS%{fs-sep}Main.swift %t%{fs-sep}VFS%{fs-sep}LibraryExt.swift %t%{fs-sep}VFS%{fs-sep}Library.swift == \
// RUN:   -req=complete.close -pos=2:9 -name %t%{fs-sep}VFS%{fs-sep}Main.swift %s == \

// RUN:   -shell -- echo "### Keep" == \
// RUN:   -req=global-config -req-opts=completion_check_dependency_interval=100 == \
// RUN:   -req=complete.open -pos=2:9 -pass-as-sourcetext -vfs-files=%t%{fs-sep}VFS%{fs-sep}Main.swift=@%s,%t%{fs-sep}VFS%{fs-sep}Library.swift=@%S/Inputs/checkdeps/MyProject_mod/Library.swift %t%{fs-sep}VFS%{fs-sep}Main.swift -- -target %target-triple %t%{fs-sep}VFS%{fs-sep}Main.swift %t%{fs-sep}VFS%{fs-sep}LibraryExt.swift %t%{fs-sep}VFS%{fs-sep}Library.swift == \
// RUN:   -req=complete.close -pos=2:9 -name %t%{fs-sep}VFS%{fs-sep}Main.swift %s \
//
// RUN:   | %FileCheck %s

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
