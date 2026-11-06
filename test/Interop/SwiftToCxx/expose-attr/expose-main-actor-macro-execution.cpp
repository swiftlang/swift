// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/expose-main-actor-macro.swift -module-name MainActorMacro -target %target-swift-5.1-abi-triple -plugin-path %swift-plugin-dir -parse-as-library -clang-header-expose-decls=has-expose-attr -typecheck -verify -emit-clang-header-path %t/macro.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/expose-main-actor-macro-execution.o
// RUN: %target-interop-build-swift %S/expose-main-actor-macro.swift -o %t/expose-main-actor-macro-execution -Xlinker %t/expose-main-actor-macro-execution.o -module-name MainActorMacro -target %target-swift-5.1-abi-triple -parse-as-library -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/expose-main-actor-macro-execution
// RUN: %target-run %t/expose-main-actor-macro-execution | %FileCheck %s

// The thunks trap when they are called off the main thread.
// DEFINE: %{run-off-main} = env %env-SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=legacy %env-SWIFT_BACKTRACE=enable=no %{python} %S/../../../Inputs/not.py
// RUN: %{run-off-main} "%target-run %t/expose-main-actor-macro-execution function" 2>&1 | %FileCheck %s --check-prefix=RACE
// RUN: %{run-off-main} "%target-run %t/expose-main-actor-macro-execution method" 2>&1 | %FileCheck %s --check-prefix=RACE
// RUN: %{run-off-main} "%target-run %t/expose-main-actor-macro-execution getter" 2>&1 | %FileCheck %s --check-prefix=RACE
// RUN: %{run-off-main} "%target-run %t/expose-main-actor-macro-execution init" 2>&1 | %FileCheck %s --check-prefix=RACE
// RUN: %{run-off-main} "%target-run %t/expose-main-actor-macro-execution subscript" 2>&1 | %FileCheck %s --check-prefix=RACE

// RACE: expose-main-actor-macro.swift:{{[0-9]+}}: Fatal error: Incorrect actor executor assumption
// RACE-NOT: returned from the background thread

// In the Swift 6 checking mode, the runtime traps in the executor's own check.
// RUN: env %env-SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=swift6 %env-SWIFT_BACKTRACE=enable=no %{python} %S/../../../Inputs/not.py "%target-run %t/expose-main-actor-macro-execution method" 2>&1 | %FileCheck %s --check-prefix=SWIFT6 --allow-empty
// SWIFT6-NOT: returned from the background thread

// NOTE: not.py is used above instead of "not --crash" because simctl's exit
// status doesn't reflect whether its child process crashed or not.

// REQUIRES: executable_test
// REQUIRES: swift_swift_parser
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

#include <cassert>
#include <cstdio>
#include <cstring>
#include <thread>

#include "macro.h"

using namespace MainActorMacro;

static void callOffMainThread(const char *kind) {
  auto object = MainActorClass::init(1);
  auto partial = PartiallyIsolatedStruct::init(1);
  std::thread thread([&] {
    // Nonisolated members can be called from any thread.
    assert(object.nonisolatedMethod() == 42);

    if (!strcmp(kind, "function"))
      mainActorFunc(1);
    else if (!strcmp(kind, "method"))
      object.method();
    else if (!strcmp(kind, "getter"))
      object.getValue();
    else if (!strcmp(kind, "init"))
      MainActorStruct::init();
    else if (!strcmp(kind, "subscript"))
      (void)partial[0];
  });
  thread.join();
  puts("returned from the background thread");
}

int main(int argc, char **argv) {
  if (argc > 1) {
    callOffMainThread(argv[1]);
    return 0;
  }

  // Calls from the main thread pass the isolation check.
  assert(mainActorFunc(1) == 2);
  swift::Int y = 0;
  renamedFunc(5, y);
  assert(y == 5);
  assert(genericFunc<swift::Int>(3) == 3);
  auto ns = NonSendable::init();
  (void)roundTrip(ns);

  auto partial = PartiallyIsolatedStruct::init(10);
  assert(partial.method() == 10);
  partial.mutatingMethod();
  assert(partial.getX() == 11);
  assert(PartiallyIsolatedStruct::staticMethod() == 42);
  partial.setComputed(20);
  assert(partial.getComputed() == 20);
  assert(partial.getReadOnly() == 20);
  PartiallyIsolatedStruct::setStaticProp(7);
  assert(PartiallyIsolatedStruct::getStaticProp() == 7);
  assert(partial.extensionMethod() == 20);
  assert(partial[1] == 21);

  // The thunk dispatches dynamically to the override.
  auto base = Base::init();
  Base derived = Derived::init();
  assert(base.virtualMethod() == 1);
  assert(derived.virtualMethod() == 2);
  assert(Base::classMethod() == 1);

  auto object = MainActorClass::init(1);
  assert(object.getValue() == 1);
  object.setValue(2);
  assert(object.method() == 2);
  object.setObject(ns);
  (void)object.getObject();

  auto value = MainActorStruct::init();
  value.increment();
  assert(value.getX() == 1);
  value.setX(5);
  assert(value.extensionMethod() == 5);
  assert(value[2] == 7);
  value.setExtensionProperty(8);
  assert(value.getExtensionProperty() == 8);

  puts("OK");
  // CHECK: OK
  return 0;
}
