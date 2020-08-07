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
// REQUIRES: shell

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Frameworks)
// RUN: %empty-directory(%t/MyProject)

// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: COMPILER_ARGS=( \
// RUN:   -target %target-triple \
// RUN:   -module-name MyProject \
// RUN:   -F %t/Frameworks \
// RUN:   -I %t/MyProject \
// RUN:   -import-objc-header %t/MyProject/Bridging.h \
// RUN:   %t/MyProject/Library.swift \
// RUN:   %t/test.swift \
// RUN: )
// RUN: INPUT_DIR=%S/Inputs/checkdeps
// RUN: DEPCHECK_INTERVAL=1
// RUN: SLEEP_TIME=2

// RUN: cp -R $INPUT_DIR/MyProject %t/
// RUN: cp -R $INPUT_DIR/ClangFW.framework %t/Frameworks/
// RUN: %empty-directory(%t/Frameworks/SwiftFW.framework/Modules/SwiftFW.swiftmodule)
// RUN: %target-swift-frontend -emit-module -module-name SwiftFW -o %t/Frameworks/SwiftFW.framework/Modules/SwiftFW.swiftmodule/%target-swiftmodule-name $INPUT_DIR/SwiftFW_src/Funcs.swift

// RUN: cp %t/State1.swift %t/test.swift

// RUN: %sourcekitd-test \
// RUN:   -req=global-config -completion-check-dependency-interval ${DEPCHECK_INTERVAL} == \

// RUN:   -shell -- echo "### Initial" == \
// RUN:   -req=complete -pos=5:4 %t/test.swift -- ${COMPILER_ARGS[@]} == \

// RUN:   -shell -- sleep ${SLEEP_TIME} == \
// RUN:   -shell -- echo "### Modify own file - 1" == \
// RUN:   -shell -- cp %t/State2.swift %t/test.swift == \
// RUN:   -req=complete -pos=5:9 %t/test.swift -- ${COMPILER_ARGS[@]} == \

// RUN:   -shell -- sleep ${SLEEP_TIME} == \
// RUN:   -shell -- echo "### Modify own file - 2" == \
// RUN:   -shell -- cp %t/State1.swift %t/test.swift == \
// RUN:   -req=complete -pos=5:4 %t/test.swift -- ${COMPILER_ARGS[@]} == \

// RUN:   -shell -- sleep ${SLEEP_TIME} == \
// RUN:   -shell -- echo "### Modify own file - 3" == \
// RUN:   -shell -- cp %t/State2.swift %t/test.swift == \
// RUN:   -req=complete -pos=5:9 %t/test.swift -- ${COMPILER_ARGS[@]} \
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
