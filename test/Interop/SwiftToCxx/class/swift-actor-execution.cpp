// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-actor-in-cxx.swift -module-name Actor -clang-header-expose-decls=has-expose-attr -typecheck -verify -emit-clang-header-path %t/actor.h -disable-availability-checking

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-actor-execution.o
// RUN: %target-interop-build-swift %S/swift-actor-in-cxx.swift -o %t/swift-actor-execution -Xlinker %t/swift-actor-execution.o -module-name Actor -Xfrontend -entry-point-function-name -Xfrontend swiftMain -Xfrontend -disable-availability-checking

// RUN: %target-codesign %t/swift-actor-execution
// RUN: %target-run %t/swift-actor-execution | %FileCheck %s

// RUN: %empty-directory(%t-evo)

// RUN: %target-swift-frontend %S/swift-actor-in-cxx.swift -module-name Actor -clang-header-expose-decls=has-expose-attr -enable-library-evolution -typecheck -verify -emit-clang-header-path %t-evo/actor.h -disable-availability-checking

// RUN: %target-interop-build-clangxx -c %s -I %t-evo -o %t-evo/swift-actor-execution.o
// RUN: %target-interop-build-swift %S/swift-actor-in-cxx.swift -o %t-evo/swift-actor-execution-evo -Xlinker %t-evo/swift-actor-execution.o -module-name Actor -enable-library-evolution -Xfrontend -entry-point-function-name -Xfrontend swiftMain -Xfrontend -disable-availability-checking

// RUN: %target-codesign %t-evo/swift-actor-execution-evo
// RUN: %target-run %t-evo/swift-actor-execution-evo | %FileCheck %s

// REQUIRES: executable_test

// REQUIRES: concurrency

#include <assert.h>
#include "actor.h"
#include <cstdio>

extern "C" size_t swift_retainCount(void * _Nonnull obj);

size_t getRetainCount(const Actor::ActorWithField & swiftClass) {
  void *p = swift::_impl::_impl_RefCountedClass::getOpaquePointer(swiftClass);
  return swift_retainCount(p);
}

int main() {
  using namespace Actor;

  // Ensure that the actor class is released.
  {
    auto x = ActorWithField::init();
    assert(getRetainCount(x) == 1);
  }
// CHECK:      init ActorWithField
// CHECK-NEXT: destroy ActorWithField
    
  {
    auto x = ActorWithField::init();
    {
      takeActorWithIntField(x);
      assert(getRetainCount(x) == 1);
    }
    assert(getRetainCount(x) == 1);
  }
// CHECK-NEXT: init ActorWithField
// CHECK-NEXT: takeActorWithIntField
// CHECK-NEXT: destroy ActorWithField
    
  {
    auto x = ActorWithField::init();
    x.method();
  }
// CHECK-NEXT: init ActorWithField
// CHECK-NEXT: nonisolated method
// CHECK-NEXT: destroy ActorWithField
  return 0;
}
