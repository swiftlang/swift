// REQUIRES: rdar60881337
import Foo

func test() {

}

// UNSUPPORTED: OS=windows-msvc

// -----------------------------------------------------------------------------
// Test that modifications for frameworks in '-Fsystem' doesn't affect the result.

// RUN: %empty-directory(%t/ModuleCache)
// RUN: %empty-directory(%t/System/Frameworks)
// RUN: cp -R %S/../Inputs/build_session/Frameworks/Foo.framework %t/System/Frameworks/
// RUN: cp -R %S/../Inputs/build_session/Frameworks/FooHelper.framework %t/System/Frameworks/
// RUN: %sourcekitd-test \
// RUN:   -shell -- echo '## ONE' == \
// RUN:   -req=complete -pos=4:1 %s -- %s -D ONE -Fsystem %t/System/Frameworks -module-cache-path %t/ModuleCache == \
// RUN:   -shell -- cp -R %S/../Inputs/build_session/Frameworks_modified/Foo.framework %t/System/Frameworks/ == \
// RUN:   -shell -- cp -R %S/../Inputs/build_session/Frameworks_modified/FooHelper.framework %t/System/Frameworks/ == \
// RUN:   -shell -- echo '## TWO' == \
// RUN:   -req=complete -pos=4:1 %s -- %s -D TWO -Fsystem %t/System/Frameworks -module-cache-path %t/ModuleCache \
// RUN:   | tee %t.response |  %FileCheck %s --check-prefix=CHECK_SYSTEM
// RUN: sleep 2
// RUN: %sourcekitd-test \
// RUN:   -shell -- echo '## THREE' == \
// RUN:   -req=complete -pos=4:1 %s -- %s -D TWO -Fsystem %t/System/Frameworks -module-cache-path %t/ModuleCache  \
// RUN:   | %FileCheck %s --check-prefix=CHECK_SYSTEM_2

// CHECK_SYSTEM-LABEL: ## ONE
// CHECK_SYSTEM-DAG: key.description: "fooFunc(arg: Int32)"
// CHECK_SYSTEM-DAG: key.description: "fooSubFunc(arg: Int32)"
// CHECK_SYSTEM-DAG: key.description: "fooHelperFunc(arg: Int32)"
// CHECK_SYSTEM-DAG: key.description: "fooHelperSubFunc(arg: Int32)"

// CHECK_SYSTEM-LABEL: ## TWO
// CHECK_SYSTEM-DAG: key.description: "fooFunc(arg: Int32)"
// CHECK_SYSTEM-DAG: key.description: "fooSubFunc(arg: Int32)"
// CHECK_SYSTEM-DAG: key.description: "fooHelperFunc(arg: Int32)"
// CHECK_SYSTEM-DAG: key.description: "fooHelperSubFunc(arg: Int32)"

// CHECK_SYSTEM_2-LABEL: ## THREE
// CHECK_SYSTEM_2-NOT: fooFunc(
// CHECK_SYSTEM_2-NOT: fooSubFunc(
// CHECK_SYSTEM_2-NOT: fooHelperFunc(
// CHECK_SYSTEM_2-NOT: fooHelperSubFunc(
// CHECK_SYSTEM_2-DAG: key.description: "fooFunc_mod(arg: Int32)"
// CHECK_SYSTEM_2-DAG: key.description: "fooSubFunc_mod(arg: Int32)"
// CHECK_SYSTEM_2-DAG: key.description: "fooHelperFunc_mod(arg: Int32)"
// CHECK_SYSTEM_2-DAG: key.description: "fooHelperSubFunc_mod(arg: Int32)"
// CHECK_SYSTEM_2-NOT: fooFunc(
// CHECK_SYSTEM_2-NOT: fooSubFunc(
// CHECK_SYSTEM_2-NOT: fooHelperFunc(
// CHECK_SYSTEM_2-NOT: fooHelperSubFunc(

// -----------------------------------------------------------------------------
// Test that modifications for frameworks in '-F' are immidiately propagated
// while modifications for frameworks in '-Fsystem' are not.

// RUN: %empty-directory(%t/ModuleCache)
// RUN: %empty-directory(%t/Frameworks)
// RUN: %empty-directory(%t/System/Frameworks)
// RUN: cp -R %S/../Inputs/build_session/Frameworks/Foo.framework %t/Frameworks/
// RUN: cp -R %S/../Inputs/build_session/Frameworks/FooHelper.framework %t/System/Frameworks/
// RUN: %sourcekitd-test \
// RUN:   -shell -- echo '## ONE' == \
// RUN:   -req=complete -pos=4:1 %s -- %s -D ONE -F %t/Frameworks -Fsystem %t/System/Frameworks -module-cache-path %t/ModuleCache  == \
// RUN:   -shell -- cp -R %S/../Inputs/build_session/Frameworks_modified/Foo.framework %t/Frameworks/ == \
// RUN:   -shell -- cp -R %S/../Inputs/build_session/Frameworks_modified/FooHelper.framework %t/System/Frameworks/ == \
// RUN:   -shell -- echo '## TWO' == \
// RUN:   -req=complete -pos=4:1 %s -- %s -D TWO -F %t/Frameworks -Fsystem %t/System/Frameworks -module-cache-path %t/ModuleCache  \
// RUN:   | %FileCheck %s --check-prefix=CHECK_USER

// CHECK_USER-LABEL: ## ONE
// CHECK_USER-DAG: key.description: "fooFunc(arg: Int32)"
// CHECK_USER-DAG: key.description: "fooSubFunc(arg: Int32)"
// CHECK_USER-DAG: key.description: "fooHelperFunc(arg: Int32)"
// CHECK_USER-DAG: key.description: "fooHelperSubFunc(arg: Int32)"

// CHECK_USER-LABEL: ## TWO
// CHECK_USER-NOT: fooFunc(
// CHECK_USER-NOT: fooSubFunc(
// CHECK_USER-DAG: key.description: "fooFunc_mod(arg: Int32)"
// CHECK_USER-DAG: key.description: "fooSubFunc_mod(arg: Int32)"
// CHECK_USER-DAG: key.description: "fooHelperFunc(arg: Int32)"
// CHECK_USER-DAG: key.description: "fooHelperSubFunc(arg: Int32)"
// CHECK_USER-NOT: fooFunc(
// CHECK_USER-NOT: fooSubFunc(
