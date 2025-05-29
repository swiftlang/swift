// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/generic-struct-in-cxx.swift -module-name Generics -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/generics.h

// RUN: %target-interop-build-clangxx -std=gnu++20 -c %s -I %t -o %t/swift-generics-execution.o
// RUN: %target-interop-build-swift %S/generic-struct-in-cxx.swift -o %t/swift-generics-execution -Xlinker %t/swift-generics-execution.o -module-name Generics -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-generics-execution
// RUN: %target-run %t/swift-generics-execution | %FileCheck %s

// REQUIRES: executable_test

#include "generics.h"
#include <cassert>
#include <cstdio>

int main() {
  using namespace Generics;

  {
    auto x = makeGenericPair<int, int>(11, 42);
    takeGenericPair(x);
    // CHECK: GenericPair<Int32, Int32>(x: 11, y: 42)
    auto xprime = passThroughGenericPair(x, -995);
    takeGenericPair(x);
    takeGenericPair(xprime);
    // CHECK-NEXT: GenericPair<Int32, Int32>(x: 11, y: 42)
    // CHECK-NEXT: GenericPair<Int32, Int32>(x: 11, y: -995)
    inoutGenericPair(x, 0xFF);
    takeGenericPair(x);
    // CHECK-NEXT: GenericPair<Int32, Int32>(x: 255, y: 42)
    x.method();
    // CHECK-NEXT: GenericPair<T, T2>::testme::255,42;
    x.mutatingMethod(xprime);
    x.method();
    // CHECK-NEXT: GenericPair<T, T2>::testme::-995,11;
    takeGenericPair(x);
    takeGenericPair(xprime);
    // CHECK-NEXT: GenericPair<Int32, Int32>(x: -995, y: 11)
    // CHECK-NEXT: GenericPair<Int32, Int32>(x: 11, y: -995)
    assert(x.getY() == 11);
    x.setY(561);
    takeGenericPair(x);
    // CHECK-NEXT: GenericPair<Int32, Int32>(x: -995, y: 561)
    assert(x.getComputedProp() == 42);
    assert(x.getComputedVar() == -995);
    x.setComputedVar(-123456);
    assert(x.getComputedVar() == -123456);
    assert(x.getY() == 561);
    takeGenericPair(x);
    // CHECK-NEXT: GenericPair<T, T2>::computeVar::get
    // CHECK-NEXT: GenericPair<T, T2>::computeVar::set
    // CHECK-NEXT: GenericPair<T, T2>::computeVar::get
    // CHECK-NEXT: GenericPair<Int32, Int32>(x: -123456, y: 561)
    assert(x.genericMethod<float>(2.25f, 4221) == 2.25f);
    // CHECK-NEXT: GenericPair<T, T2>::genericMethod<T>::2.25,4221;
  }

  {
    auto x = makeConcretePair(10000, 0xfee7);
    takeGenericPair(x);
    // CHECK-NEXT: GenericPair<UInt16, UInt16>(x: 10000, y: 65255)
    takeConcretePair(x);
    // CHECK-NEXT: CONCRETE pair of UInt16: 10000 65255 ;
    auto xprime = passThroughConcretePair(x, 918);
    takeConcretePair(x);
    takeConcretePair(xprime);
    takeGenericPair(xprime);
    // CHECK-NEXT: CONCRETE pair of UInt16: 10000 65255 ;
    // CHECK-NEXT: CONCRETE pair of UInt16: 10000 918 ;
    // CHECK-NEXT: GenericPair<UInt16, UInt16>(x: 10000, y: 918)
    inoutConcretePair(77, x);
    takeConcretePair(x);
    // CHECK-NEXT: CONCRETE pair of UInt16: 77 65255 ;
    x.method();
    // CHECK-NEXT: GenericPair<T, T2>::testme::77,65255;
    x.mutatingMethod(xprime);
    x.method();
    // CHECK-NEXT: GenericPair<T, T2>::testme::918,10000;
    takeConcretePair(xprime);
    // CHECK-NEXT: CONCRETE pair of UInt16: 10000 918 ;
  }

  {
    auto x = GenericPair<int, int>::init(11, 234242, 44);
    takeGenericPair(x);
    x.method();
    assert(x.getY() == 44);
    // CHECK-NEXT: GenericPair<T, T2>::init::11,44,234242;
    // CHECK-NEXT: GenericPair<Int32, Int32>(x: 11, y: 44)
    // CHECK-NEXT: GenericPair<T, T2>::testme::11,44;
    auto y = GenericPair<uint16_t, uint16_t>::init(0, -987, 3425);
    takeConcretePair(y);
    y.method();
    assert(y.getY() == 3425);
    // CHECK-NEXT: GenericPair<T, T2>::init::0,3425,-987;
    // CHECK-NEXT: CONCRETE pair of UInt16: 0 3425 ;
    // CHECK-NEXT: GenericPair<T, T2>::testme::0,3425;
    auto val = PairOfUInt64::init(0xafafa, 0x32443);
    auto valprime = x.genericMethod(val, 4221);
    assert(valprime.getX() == val.getX());
    assert(valprime.getY() == val.getY());
    // CHECK-NEXT: GenericPair<T, T2>::genericMethod<T>::PairOfUInt64(x: 719610, y: 205891),4221;
  }
  puts("EOF\n");
  // CHECK-NEXT: EOF
  return 0;
}
