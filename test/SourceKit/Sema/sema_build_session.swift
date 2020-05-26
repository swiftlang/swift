import Foo
import FooHelper.FooHelperExplicit

func swiftFunc() -> Int { 1 }

func test() {
  _ = fooFunc(1)
  _ = fooSubFunc(1)
  _ = fooHelperFunc(1)
  _ = fooHelperSubFunc(1)
  _ = fooHelperExplicitFunc(1)
  _ = swiftFunc()
}

// UNSUPPORTED: OS=windows-msvc

// -----------------------------------------------------------------------------
// Test that modifications for frameworks in '-Fsystem' doesn't affect the result
// within a session, and that they are propagated after restarting SourceKit.

// RUN: %empty-directory(%t/ModuleCache)
// RUN: %empty-directory(%t/System/Frameworks)
// RUN: cp -R %S/../Inputs/build_session/Frameworks/Foo.framework %t/System/Frameworks/
// RUN: cp -R %S/../Inputs/build_session/Frameworks/FooHelper.framework %t/System/Frameworks/
// RUN: %sourcekitd-test \
// RUN:   -shell -- echo '## ONE' == \
// RUN:   -req=sema %s -- %s -D ONE -Fsystem %t/System/Frameworks -module-cache-path %t/ModuleCache == \
// RUN:   -shell -- cp -R %S/../Inputs/build_session/Frameworks_modified/Foo.framework %t/System/Frameworks/ == \
// RUN:   -shell -- cp -R %S/../Inputs/build_session/Frameworks_modified/FooHelper.framework %t/System/Frameworks/ == \
// RUN:   -shell -- echo '## TWO' == \
// RUN:   -req=sema %s -- %s -D TWO -Fsystem %t/System/Frameworks -module-cache-path %t/ModuleCache  \
// RUN:   | %FileCheck %s --check-prefix=CHECK_SYSTEM
// RUN: sleep 2
// RUN: %sourcekitd-test \
// RUN:   -shell -- echo '## THREE' == \
// RUN:   -req=sema %s -- %s -D THREE -Fsystem %t/System/Frameworks -module-cache-path %t/ModuleCache  \
// RUN:   | %FileCheck %s --check-prefix=CHECK_SYSTEM_2

// CHECK_SYSTEM-LABEL: ## ONE
// CHECK_SYSTEM-NOT: key.description

// CHECK_SYSTEM-LABEL: ## TWO
// CHECK_SYSTEM-NOT: key.description

// CHECK_SYSTEM_2-LABEL: ## THREE
// CHECK_SYSTEM_2: key.severity: source.diagnostic.severity.error,
// CHECK_SYSTEM_2-NEXT: key.description: "cannot find 'fooFunc' in scope",
// CHECK_SYSTEM_2: key.severity: source.diagnostic.severity.error,
// CHECK_SYSTEM_2-NEXT: key.description: "cannot find 'fooSubFunc' in scope",
// CHECK_SYSTEM_2: key.severity: source.diagnostic.severity.error,
// CHECK_SYSTEM_2-NEXT: key.description: "cannot find 'fooHelperFunc' in scope",
// CHECK_SYSTEM_2: key.severity: source.diagnostic.severity.error,
// CHECK_SYSTEM_2-NEXT: key.description: "cannot find 'fooHelperSubFunc' in scope",
// CHECK_SYSTEM_2: key.severity: source.diagnostic.severity.error,
// CHECK_SYSTEM_2-NEXT: key.description: "cannot find 'fooHelperExplicitFunc' in scope",

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
// RUN:   -req=sema %s -- %s -D ONE -F %t/Frameworks -Fsystem %t/System/Frameworks -module-cache-path %t/ModuleCache  == \
// RUN:   -shell -- cp -R %S/../Inputs/build_session/Frameworks_modified/Foo.framework %t/Frameworks/ == \
// RUN:   -shell -- cp -R %S/../Inputs/build_session/Frameworks_modified/FooHelper.framework %t/System/Frameworks/ == \
// RUN:   -shell -- echo '## TWO' == \
// RUN:   -req=sema %s -- %s -D TWO -F %t/Frameworks -Fsystem %t/System/Frameworks -module-cache-path %t/ModuleCache  \
// RUN:   | %FileCheck %s --check-prefix=CHECK_USER

// CHECK_USER-LABEL: ## ONE
// CHECK_USER-NOT: key.description

// CHECK_USER-LABEL: ## TWO
// CHECK_USER-NOT: key.severity: 
// CHECK_USER: key.severity: source.diagnostic.severity.error,
// CHECK_USER-NEXT: key.description: "cannot find 'fooFunc' in scope",
// CHECK_USER: key.severity: source.diagnostic.severity.error,
// CHECK_USER-NEXT: key.description: "cannot find 'fooSubFunc' in scope",
// CHECK_USER-NOT: key.severity: 

// -----------------------------------------------------------------------------
// Test that modifications for frameworks in '-Fsystem' doesn't affect the result
// across SourceKit sessions *if* '-disable-modules-validate-system-headers' is
// passed.

// RUN: %empty-directory(%t/ModuleCache)
// RUN: %empty-directory(%t/System/Frameworks)
// RUN: cp -R %S/../Inputs/build_session/Frameworks/Foo.framework %t/System/Frameworks/
// RUN: cp -R %S/../Inputs/build_session/Frameworks/FooHelper.framework %t/System/Frameworks/
// RUN: %sourcekitd-test \
// RUN:   -shell -- echo '## ONE' == \
// RUN:   -req=sema %s -- %s -D ONE -Fsystem %t/System/Frameworks -module-cache-path %t/ModuleCache -Xfrontend -disable-modules-validate-system-headers == \
// RUN:   -shell -- cp -R %S/../Inputs/build_session/Frameworks_modified/Foo.framework %t/System/Frameworks/ == \
// RUN:   -shell -- cp -R %S/../Inputs/build_session/Frameworks_modified/FooHelper.framework %t/System/Frameworks/ == \
// RUN:   -shell -- echo '## TWO' == \
// RUN:   -req=sema %s -- %s -D TWO -Fsystem %t/System/Frameworks -module-cache-path %t/ModuleCache -Xfrontend -disable-modules-validate-system-headers \
// RUN:   | %FileCheck %s --check-prefix=CHECK_DISABLED
// RUN: sleep 2
// RUN: %sourcekitd-test \
// RUN:   -shell -- echo '## THREE' == \
// RUN:   -req=sema %s -- %s -D THREE -Fsystem %t/System/Frameworks -module-cache-path %t/ModuleCache -Xfrontend -disable-modules-validate-system-headers \
// RUN:   | %FileCheck %s --check-prefix=CHECK_DISABLED_2

// CHECK_DISABLED-LABEL: ## ONE
// CHECK_DISABLED-NOT: key.description

// CHECK_DISABLED-LABEL: ## TWO
// CHECK_DISABLED-NOT: key.description

// CHECK_DISABLED_2-LABEL: ## THREE
// CHECK_DISABLED_2-NOT: key.description
