import ClangFW
import SwiftFW

func foo() {
  /*HERE*/
}

// UNSUPPORTED: OS=windows-msvc
// TODO: Try to make this test work on Windows when reenabling it.
// REQUIRES: rdar74150023

// RUN: %empty-directory(%t/Frameworks)
// RUN: %empty-directory(%t/MyProject)

// DEFINE: %{args} = \
// DEFINE:   -target %target-triple \
// DEFINE:   -module-name MyProject \
// DEFINE:   -F %t/Frameworks \
// DEFINE:   -I %t/MyProject \
// DEFINE:   -import-objc-header %t/MyProject/Bridging.h \
// DEFINE:   %t/MyProject/Library.swift \
// DEFINE:   %s

// RUN: cp -R %S/Inputs/checkdeps/MyProject %t/
// RUN: cp -R %S/Inputs/checkdeps/ClangFW.framework %t/Frameworks/
// RUN: %empty-directory(%t/Frameworks/SwiftFW.framework/Modules/SwiftFW.swiftmodule)
// RUN: %target-swift-frontend -emit-module -module-name SwiftFW -o %t/Frameworks/SwiftFW.framework/Modules/SwiftFW.swiftmodule/%target-swiftmodule-name %S/Inputs/checkdeps/SwiftFW_src/Funcs.swift

// RUN: %sourcekitd-test \
// RUN:   -shell -- echo "### Initial" == \
// RUN:   -req=complete -pos=5:3 %s -- %{args} == \

// RUN:   -shell -- echo '### Modify framework' == \
// RUN:   -shell -- %target-swift-frontend -emit-module -module-name SwiftFW -o %t/Frameworks/SwiftFW.framework/Modules/SwiftFW.swiftmodule/%target-swiftmodule-name %S/Inputs/checkdeps/SwiftFW_src_mod/Funcs.swift == \
// RUN:   -req=dependency-updated -- %{args} == \
// RUN:   -req=complete -pos=5:3 %s -- %{args} == \

// RUN:   -shell -- echo '### Notify without modifying' == \
// RUN:   -req=dependency-updated -- %{args} == \
// RUN:   -req=complete -pos=5:3 %s -- %{args} \

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
