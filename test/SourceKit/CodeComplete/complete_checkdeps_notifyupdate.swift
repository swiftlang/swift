import ClangFW
import SwiftFW

func foo() {
  /*HERE*/
}

// REQUIRES: shell
// REQUIRES: rdar74150023

// RUN: %empty-directory(%t/Frameworks)
// RUN: %empty-directory(%t/MyProject)

// RUN: COMPILER_ARGS=( \
// RUN:   -target %target-triple \
// RUN:   -module-name MyProject \
// RUN:   -F %t/Frameworks \
// RUN:   -I %t/MyProject \
// RUN:   -import-objc-header %t/MyProject/Bridging.h \
// RUN:   %t/MyProject/Library.swift \
// RUN:   %s \
// RUN: )
// RUN: INPUT_DIR=%S/Inputs/checkdeps

// RUN: cp -R $INPUT_DIR/MyProject %t/
// RUN: cp -R $INPUT_DIR/ClangFW.framework %t/Frameworks/
// RUN: %empty-directory(%t/Frameworks/SwiftFW.framework/Modules/SwiftFW.swiftmodule)
// RUN: %target-swift-frontend -emit-module -module-name SwiftFW -o %t/Frameworks/SwiftFW.framework/Modules/SwiftFW.swiftmodule/%target-swiftmodule-name $INPUT_DIR/SwiftFW_src/Funcs.swift

// RUN: %sourcekitd-test \
// RUN:   -shell -- echo "### Initial" == \
// RUN:   -req=complete -pos=5:3 %s -- ${COMPILER_ARGS[@]} == \

// RUN:   -shell -- echo '### Modify framework' == \
// RUN:   -shell -- %target-swift-frontend -emit-module -module-name SwiftFW -o %t/Frameworks/SwiftFW.framework/Modules/SwiftFW.swiftmodule/%target-swiftmodule-name $INPUT_DIR/SwiftFW_src_mod/Funcs.swift == \
// RUN:   -req=dependency-updated -- ${COMPILER_ARGS[@]} == \
// RUN:   -req=complete -pos=5:3 %s -- ${COMPILER_ARGS[@]} == \

// RUN:   -shell -- echo '### Notify without modifying' == \
// RUN:   -req=dependency-updated -- ${COMPILER_ARGS[@]} == \
// RUN:   -req=complete -pos=5:3 %s -- ${COMPILER_ARGS[@]} \

// RUN:   | %FileCheck %s

// CHECK-LABEL: ### Initial
// CHECK: key.results: [
// CHECK-DAG: key.description: "clangFWFunc()"
// CHECK-DAG: key.description: "swiftFWFunc()"
// CHECK-DAG: key.description: "localClangFunc()"
// CHECK-DAG: key.description: "localSwiftFunc()"
// CHECK: ]
// CHECK-NOT: key.reusingastcontext: 1

// CHECK-LABEL: ### Modify framework
// CHECK: key.results: [
// CHECK-DAG: key.description: "clangFWFunc()"
// CHECK-DAG: key.description: "swiftFWFunc_mod()"
// CHECK-DAG: key.description: "localClangFunc()"
// CHECK-DAG: key.description: "localSwiftFunc()"
// CHECK: ]
// CHECK-NOT: key.reusingastcontext: 1

// CHECK-LABEL: ### Notify without modifying
// CHECK: key.results: [
// CHECK-DAG: key.description: "clangFWFunc()"
// CHECK-DAG: key.description: "swiftFWFunc_mod()"
// CHECK-DAG: key.description: "localClangFunc()"
// CHECK-DAG: key.description: "localSwiftFunc()"
// CHECK: ]
// CHECK-NOT: key.reusingastcontext: 1
