// REQUIRES: rdar72466352

import ClangFW
import SwiftFW

func foo() {
  /*HERE*/
}

// UNSUPPORTED: OS=windows-msvc
// TODO: Try to make this test work on Windows when reenabling it.

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
// RUN: touch -t 202001010101 %t/Frameworks/ClangFW.framework/Headers/*
// RUN: %empty-directory(%t/Frameworks/SwiftFW.framework/Modules/SwiftFW.swiftmodule)
// RUN: %target-swift-frontend -emit-module -module-name SwiftFW -o %t/Frameworks/SwiftFW.framework/Modules/SwiftFW.swiftmodule/%target-swiftmodule-name %S/Inputs/checkdeps/SwiftFW_src/Funcs.swift

// RUN: %sourcekitd-test \
// RUN:   -req=global-config -req-opts=completion_check_dependency_interval=0 == \

// RUN:   -shell -- echo "### Initial" == \
// RUN:   -req=complete -pos=5:3 %s -- %{args} == \

// RUN:   -shell -- echo '### Modify framework (c)' == \
// RUN:   -shell -- cp -R %S/Inputs/checkdeps/ClangFW.framework_mod/* %t/Frameworks/ClangFW.framework/ == \
// RUN:   -shell -- touch -t 210001010101 %t/Frameworks/ClangFW.framework/Headers/* == \
// RUN:   -req=complete -pos=5:3 %s -- %{args} == \

// RUN:   -shell -- echo '### Fast completion' == \
// RUN:   -shell -- touch -t 202001010101 %t/Frameworks/ClangFW.framework/Headers/* == \
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

// CHECK-LABEL: ### Modify framework (c)
// CHECK: key.results: [
// CHECK-DAG: key.description: "clangFWFunc_mod()"
// CHECK-DAG: key.description: "swiftFWFunc()"
// CHECK-DAG: key.description: "localClangFunc()"
// CHECK-DAG: key.description: "localSwiftFunc()"
// CHECK: ]
// CHECK-NOT: key.reusingastcontext: 1

// CHECK-LABEL: ### Fast completion
// CHECK: key.results: [
// CHECK-DAG: key.description: "clangFWFunc_mod()"
// CHECK-DAG: key.description: "swiftFWFunc()"
// CHECK-DAG: key.description: "localClangFunc()"
// CHECK-DAG: key.description: "localSwiftFunc()"
// CHECK: ]
// CHECK: key.reusingastcontext: 1
