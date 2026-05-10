// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/generic-enum-in-cxx.swift -module-name Generics -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/generics.h

// RUN: %target-interop-build-clangxx -std=gnu++20 -c %s -I %t -o %t/swift-generics-execution.o
// RUN: %target-interop-build-swift %S/generic-enum-in-cxx.swift -o %t/swift-generics-execution -Xlinker %t/swift-generics-execution.o -module-name Generics -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-generics-execution
// RUN: %target-run %t/swift-generics-execution | %FileCheck %s

// REQUIRES: executable_test

#include "generics.h"
#include <cassert>
#include <cstdio>

extern "C" size_t swift_retainCount(void * _Nonnull obj);

size_t getRetainCount(const Generics::TracksDeinit & swiftClass) {
  void *p = swift::_impl::_impl_RefCountedClass::getOpaquePointer(swiftClass);
  return swift_retainCount(p);
}

int main() {
  using namespace Generics;

  {
    auto x = makeGenericOpt<int>(42);
    takeGenericOpt(x);
    // CHECK: some(42)
    inoutGenericOpt(x, -11);
    takeGenericOpt(x);
    // CHECK-NEXT: none
    inoutGenericOpt(x, -11);
    takeGenericOpt(x);
    // CHECK-NEXT: some(-11)
    x.method();
    // CHECK-NEXT: GenericOpt<T>::testme::some(-11);
    assert(x.getComputedProp() == 42);
    assert(x.isSome());
    auto val = x.getSome();
    assert(val == -11);
    x.reset();
    assert(x.isNone());
    assert(x.genericMethod<double>(5.25) == 5.25);
    // CHECK-NEXT: GenericOpt<T>::genericMethod<T>::none,5.25;
  }
  {
    auto x = makeConcreteOpt(0xFA);
    takeConcreteOpt(x);
    // CHECK-NEXT: CONCRETE opt: some(250) ;
    inoutConcreteOpt(x);
    takeConcreteOpt(x);
    // CHECK-NEXT: CONCRETE opt: some(750) ;
    inoutConcreteOpt(x);
    takeConcreteOpt(x);
    // CHECK-NEXT: CONCRETE opt: some(2250) ;
    x.method();
    // CHECK-NEXT: GenericOpt<T>::testme::some(2250);
    assert(x.getComputedProp() == 42);
    assert(x.isSome());
    auto val = x.getSome();
    assert(val == 2250);
    x.reset();
    assert(x.isNone());
    assert(x.genericMethod<double>(-1.25) == -1.25);
    // CHECK-NEXT: GenericOpt<T>::genericMethod<T>::none,-1.25;
  }
  {
    auto x   = makeGenericOpt<StructForEnum>(StructForEnum::init());
    auto val = x.getSome();
    // CHECK-NEXT: init-TracksDeinit
    // CHECK-NEXT: destroy-TracksDeinit
  }
  {
    auto ptr = constructTracksDeinit();
    // CHECK-NEXT: init-TracksDeinit
    assert(getRetainCount(ptr) == 1);
    {
      auto x   = makeGenericOpt<TracksDeinit>(ptr);
      assert(getRetainCount(ptr) == 2);
      auto ptr2 = x.getSome();
      assert(getRetainCount(ptr) == 3);
    }
    puts("after some");
    assert(getRetainCount(ptr) == 1);
    // CHECK-NEXT: after some
    // CHECK-NEXT: destroy-TracksDeinit
  }
  {
    auto x = GenericOpt<int>::some(14);
    takeGenericOpt(x);
    assert(x.isSome());
    assert(x.getSome() == 14);
    // CHECK-NEXT: some(14)
    auto y = GenericOpt<int>::none();
    takeGenericOpt(y);
    assert(y.isNone());
    // CHECK-NEXT: none
  }
  {
    auto x = GenericOpt<StructForEnum>::some(StructForEnum::init());
    takeGenericOpt(x);
    assert(x.isSome());
    // CHECK-NEXT: init-TracksDeinit
    // CHECK-NEXT: some(Generics.StructForEnum(x: Generics.TracksDeinit))
    // CHECK-NEXT: destroy-TracksDeinit
    auto y = GenericOpt<StructForEnum>::none();
    assert(y.isNone());
  }
  {
    auto ptr = constructTracksDeinit();
    // CHECK-NEXT: init-TracksDeinit
    assert(getRetainCount(ptr) == 1);
    {
      auto x = GenericOpt<TracksDeinit>::some(ptr);
      assert(getRetainCount(ptr) == 2);
      auto ptr2 = x.getSome();
      assert(getRetainCount(ptr) == 3);
    }
    puts("after some");
    assert(getRetainCount(ptr) == 1);
    // CHECK-NEXT: after some
    // CHECK-NEXT: destroy-TracksDeinit
  }
  {
    auto opt = swift::Optional<int>::some(14);
    auto x = GenericCustomType<int>::success(opt);
    auto y = x;
    assert(y.isSuccess());
    assert(y.getSuccess().getSome() == 14);
  }
  puts("EOF");
  // CHECK-NEXT: EOF
  return 0;
}
