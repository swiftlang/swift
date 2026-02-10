// BEGIN State1.swift
import ClangFW
import SwiftFW

func foo(val: MyStruct) {
   /*HERE*/
}

// BEGIN State2.swift
import ClangFW
import SwiftFW

func foo(val: MyStruct) {
    val./*HERE*/
}

// BEGIN DUMMY.swift

// Checks that editing and saving the current file doesn't affect dependency checking.
// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Frameworks)
// RUN: %empty-directory(%t/MyProject)

// RUN: %{python} %utils/split_file.py -o %t %s

// DEFINE: %{args} = \
// DEFINE:   -target %target-triple \
// DEFINE:   -module-name MyProject \
// DEFINE:   -F %t/Frameworks \
// DEFINE:   -I %t/MyProject \
// DEFINE:   -import-objc-header %t/MyProject/Bridging.h \
// DEFINE:   %t/MyProject/Library.swift \
// DEFINE:   %t/test.swift

// RUN: cp -R %S/Inputs/checkdeps/MyProject %t/
// RUN: cp -R %S/Inputs/checkdeps/ClangFW.framework %t/Frameworks/
// RUN: %empty-directory(%t/Frameworks/SwiftFW.framework/Modules/SwiftFW.swiftmodule)
// RUN: %target-swift-frontend -emit-module -module-name SwiftFW -o %t/Frameworks/SwiftFW.framework/Modules/SwiftFW.swiftmodule/%target-swiftmodule-name %S/Inputs/checkdeps/SwiftFW_src/Funcs.swift

// RUN: cp %t/State1.swift %t/test.swift
// RUN: touch -t 202001010101 %t/test.swift

// RUN: %sourcekitd-test \
// RUN:   -req=global-config -req-opts=completion_check_dependency_interval=0 == \

// RUN:   -shell -- echo "### Initial" == \
// RUN:   -req=complete -pos=5:4 %t/test.swift -- %{args} == \

// RUN:   -shell -- echo "### Modify own file - 1" == \
// RUN:   -shell -- cp %t/State2.swift %t/test.swift == \
// RUN:   -shell -- touch -t 210001010101 %t/test.swift == \
// RUN:   -req=complete -pos=5:9 %t/test.swift -- %{args} == \

// RUN:   -shell -- echo "### Modify own file - 2" == \
// RUN:   -shell -- cp %t/State1.swift %t/test.swift == \
// RUN:   -shell -- touch -t 210001010102 %t/test.swift == \
// RUN:   -req=complete -pos=5:4 %t/test.swift -- %{args} == \

// RUN:   -shell -- echo "### Modify own file - 3" == \
// RUN:   -shell -- cp %t/State2.swift %t/test.swift == \
// RUN:   -shell -- touch -t 210001010103 %t/test.swift == \
// RUN:   -req=complete -pos=5:9 %t/test.swift -- %{args} \
// RUN:   | %FileCheck %s

// CHECK-LABEL: ### Initial
// CHECK: key.results: [
// CHECK-DAG: key.description: "clangFWFunc()"
// CHECK-DAG: key.description: "swiftFWFunc()"
// CHECK-DAG: key.description: "localClangFunc()"
// CHECK-DAG: key.description: "localSwiftFunc()"
// CHECK: ]
// CHECK-NOT: key.reusingastcontext: 1

// CHECK-LABEL: ### Modify own file - 1
// CHECK: key.results: [
// CHECK-DAG: key.description: "myStructMethod()"
// CHECK-DAG: key.description: "self"
// CHECK: ]
// CHECK: key.reusingastcontext: 1

// CHECK-LABEL: ### Modify own file - 2
// CHECK: key.results: [
// CHECK-DAG: key.description: "clangFWFunc()"
// CHECK-DAG: key.description: "swiftFWFunc()"
// CHECK-DAG: key.description: "localClangFunc()"
// CHECK-DAG: key.description: "localSwiftFunc()"
// CHECK: ]
// CHECK: key.reusingastcontext: 1

// CHECK-LABEL: ### Modify own file - 3
// CHECK: key.results: [
// CHECK-DAG: key.description: "myStructMethod()"
// CHECK-DAG: key.description: "self"
// CHECK: ]
// CHECK: key.reusingastcontext: 1
