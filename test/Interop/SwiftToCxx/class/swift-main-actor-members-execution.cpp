// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-main-actor-members.swift -module-name Members -target %target-swift-5.1-abi-triple -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/members.h

// RUN: %target-interop-build-clangxx -c %s -I %t -DSWIFT_CXX_INTEROP_ENABLE_MAIN_ACTOR -o %t/swift-main-actor-members-execution.o
// RUN: %target-interop-build-swift %S/swift-main-actor-members.swift -o %t/swift-main-actor-members-execution -Xlinker %t/swift-main-actor-members-execution.o -module-name Members -target %target-swift-5.1-abi-triple -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-main-actor-members-execution
// RUN: %target-run %t/swift-main-actor-members-execution | %FileCheck %s

// Every kind of isolated thunk traps when it is called off the main thread.
// DEFINE: %{run-off-main} = env %env-SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=legacy %env-SWIFT_UNEXPECTED_EXECUTOR_LOG_LEVEL=2 %env-SWIFT_BACKTRACE=enable=no %{python} %S/../../../Inputs/not.py
// RUN: %{run-off-main} "%target-run %t/swift-main-actor-members-execution function" 2>&1 | %FileCheck %s --check-prefix=RACE
// RUN: %{run-off-main} "%target-run %t/swift-main-actor-members-execution generic-function" 2>&1 | %FileCheck %s --check-prefix=RACE
// RUN: %{run-off-main} "%target-run %t/swift-main-actor-members-execution init" 2>&1 | %FileCheck %s --check-prefix=RACE
// RUN: %{run-off-main} "%target-run %t/swift-main-actor-members-execution method" 2>&1 | %FileCheck %s --check-prefix=RACE
// RUN: %{run-off-main} "%target-run %t/swift-main-actor-members-execution static-method" 2>&1 | %FileCheck %s --check-prefix=RACE
// RUN: %{run-off-main} "%target-run %t/swift-main-actor-members-execution extension-method" 2>&1 | %FileCheck %s --check-prefix=RACE
// RUN: %{run-off-main} "%target-run %t/swift-main-actor-members-execution generic-type-method" 2>&1 | %FileCheck %s --check-prefix=RACE
// RUN: %{run-off-main} "%target-run %t/swift-main-actor-members-execution enum-method" 2>&1 | %FileCheck %s --check-prefix=RACE
// RUN: %{run-off-main} "%target-run %t/swift-main-actor-members-execution getter" 2>&1 | %FileCheck %s --check-prefix=RACE
// RUN: %{run-off-main} "%target-run %t/swift-main-actor-members-execution setter" 2>&1 | %FileCheck %s --check-prefix=RACE
// RUN: %{run-off-main} "%target-run %t/swift-main-actor-members-execution static-property" 2>&1 | %FileCheck %s --check-prefix=RACE
// RUN: %{run-off-main} "%target-run %t/swift-main-actor-members-execution subscript" 2>&1 | %FileCheck %s --check-prefix=RACE

// RACE: error: data race detected: @MainActor function at {{.*}}members.h:{{[0-9]+}} was not called on the main thread
// RACE-NOT: returned from the background thread

// In the Swift 6 checking mode, the runtime traps in the executor's own check.
// RUN: env %env-SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=swift6 %env-SWIFT_BACKTRACE=enable=no %{python} %S/../../../Inputs/not.py "%target-run %t/swift-main-actor-members-execution method" 2>&1 | %FileCheck %s --check-prefix=SWIFT6 --allow-empty
// SWIFT6-NOT: returned from the background thread

// NOTE: not.py is used above instead of "not --crash" because simctl's exit
// status doesn't reflect whether its child process crashed or not.

// When data races are only logged, the call goes ahead.
// RUN: env %env-SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=legacy %env-SWIFT_UNEXPECTED_EXECUTOR_LOG_LEVEL=1 %target-run %t/swift-main-actor-members-execution method 2>&1 | %FileCheck %s --check-prefix=WARN
// WARN: warning: data race detected: @MainActor function at {{.*}}members.h:{{[0-9]+}} was not called on the main thread
// WARN: returned from the background thread

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

#include <cassert>
#include <cstdio>
#include <cstring>
#include <thread>

#include "members.h"

using namespace Members;

static void callOffMainThread(const char *kind) {
  // Values of main-actor-isolated types can be created on the main thread and
  // passed to other threads.
  auto object = MainActorClass::init(1);
  auto value = MainActorStruct::init();
  auto partial = PartiallyIsolatedStruct::init();
  auto box = GenericBox<swift::Int>::init(1);
  auto enumValue = MainActorEnum::first();

  std::thread thread([&] {
    // Nonisolated members can be called from any thread.
    (void)object.nonisolatedMethod();
    (void)partial.nonisolatedMethod();
    (void)box.nonisolatedGet();

    if (strcmp(kind, "function") == 0)
      (void)mainActorFunc();
    else if (strcmp(kind, "generic-function") == 0)
      (void)mainActorGenericFunc(swift::Int(1));
    else if (strcmp(kind, "init") == 0)
      (void)PartiallyIsolatedStruct::init(1);
    else if (strcmp(kind, "method") == 0)
      (void)object.increment();
    else if (strcmp(kind, "static-method") == 0)
      (void)MainActorClass::make();
    else if (strcmp(kind, "extension-method") == 0)
      (void)partial.mainActorExtensionMethod();
    else if (strcmp(kind, "generic-type-method") == 0)
      (void)box.isolatedGet();
    else if (strcmp(kind, "enum-method") == 0)
      (void)enumValue.describe();
    else if (strcmp(kind, "getter") == 0)
      (void)object.getValue();
    else if (strcmp(kind, "setter") == 0)
      object.setValue(2);
    else if (strcmp(kind, "static-property") == 0)
      (void)MainActorClass::getCounter();
    else if (strcmp(kind, "subscript") == 0)
      (void)value[1];
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
  assert(mainActorFunc() == 1);
  assert(mainActorGenericFunc(swift::Int(5)) == 5);

  auto object = MainActorClass::init(1);
  assert(object.getValue() == 1);
  object.setValue(2);
  assert(object.increment() == 3);
  assert(object.getConstant() == 7);
  assert(MainActorClass::make().getValue() == 0);
  MainActorClass::setCounter(4);
  assert(MainActorClass::getCounter() == 4);
  object.preconcurrencyMethod();
  object.renamedMethod();

  auto value = MainActorStruct::init();
  assert(value.getY() == 1);
  value.setY(2);
  assert(value[1] == 3);

  auto partial = PartiallyIsolatedStruct::init(5);
  assert(partial.getMainActorProperty() == 5);
  partial.setMainActorProperty(6);
  partial.mainActorMethod(7);
  assert(partial.nonisolatedMethod() == 7);
  assert(partial.mainActorExtensionMethod() == 7);

  assert(MainActorEnum::second().describe() == 2);
  assert(MainActorEnum::first().getComputed() == 3);

  auto box = GenericBox<swift::Int>::init(8);
  assert(box.isolatedGet() == 8);

  puts("passed");
  // CHECK: passed
  return 0;
}
