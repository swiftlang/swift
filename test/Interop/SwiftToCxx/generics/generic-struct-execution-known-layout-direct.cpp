// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/generic-struct-in-cxx.swift -D KNOWN_LAYOUT -module-name Generics -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/generics.h

// RUN: %target-interop-build-clangxx -std=gnu++20 -c %s -I %t -o %t/swift-generics-execution.o
// RUN: %target-interop-build-swift %S/generic-struct-in-cxx.swift -D KNOWN_LAYOUT -o %t/swift-generics-execution -Xlinker %t/swift-generics-execution.o -module-name Generics -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-generics-execution
// RUN: %target-run %t/swift-generics-execution | %FileCheck %s

// REQUIRES: executable_test

#include <cstdint>
#include "generic-struct-execution.cpp"

// CHECK: GenericPair<Int32, Int32>(x_: ClassWithT(11), y_: ClassWithT(42))
// CHECK-NEXT: GenericPair<Int32, Int32>(x_: ClassWithT(11), y_: ClassWithT(42))
// CHECK-NEXT: GenericPair<Int32, Int32>(x_: ClassWithT(11), y_: ClassWithT(-995))
// CHECK-NEXT: GenericPair<Int32, Int32>(x_: ClassWithT(255), y_: ClassWithT(42))
// CHECK-NEXT: GenericPair<T, T2>::testme::255,42;
// CHECK-NEXT: GenericPair<T, T2>::testme::-995,11;
// CHECK-NEXT: GenericPair<Int32, Int32>(x_: ClassWithT(-995), y_: ClassWithT(11))
// CHECK-NEXT: GenericPair<Int32, Int32>(x_: ClassWithT(11), y_: ClassWithT(-995))
// CHECK-NEXT: GenericPair<Int32, Int32>(x_: ClassWithT(-995), y_: ClassWithT(561))
// CHECK-NEXT: GenericPair<T, T2>::computeVar::get
// CHECK-NEXT: GenericPair<T, T2>::computeVar::set
// CHECK-NEXT: GenericPair<T, T2>::computeVar::get
// CHECK-NEXT: GenericPair<Int32, Int32>(x_: ClassWithT(-123456), y_: ClassWithT(561))
// CHECK-NEXT: GenericPair<T, T2>::genericMethod<T>::2.25,4221;
// CHECK-NEXT: GenericPair<UInt16, UInt16>(x_: ClassWithT(10000), y_: ClassWithT(65255))
// CHECK-NEXT: CONCRETE pair of UInt16: 10000 65255 ;
// CHECK-NEXT: CONCRETE pair of UInt16: 10000 65255 ;
// CHECK-NEXT: CONCRETE pair of UInt16: 10000 918 ;
// CHECK-NEXT: GenericPair<UInt16, UInt16>(x_: ClassWithT(10000), y_: ClassWithT(918))
// CHECK-NEXT: CONCRETE pair of UInt16: 77 65255 ;
// CHECK-NEXT: GenericPair<T, T2>::testme::77,65255;
// CHECK-NEXT: GenericPair<T, T2>::testme::918,10000;
// CHECK-NEXT: CONCRETE pair of UInt16: 10000 918 ;
// CHECK-NEXT: GenericPair<T, T2>::init::11,44,234242;
// CHECK-NEXT: GenericPair<Int32, Int32>(x_: ClassWithT(11), y_: ClassWithT(44))
// CHECK-NEXT: GenericPair<T, T2>::testme::11,44;
// CHECK-NEXT: GenericPair<T, T2>::init::0,3425,-987;
// CHECK-NEXT: CONCRETE pair of UInt16: 0 3425 ;
// CHECK-NEXT: GenericPair<T, T2>::testme::0,3425;
// CHECK-NEXT: GenericPair<T, T2>::genericMethod<T>::PairOfUInt64(x: 719610, y: 205891),4221;
// CHECK-NEXT: EOF

#if UINTPTR_MAX == 0xFFFFFFFFFFFFFFFFu
using stubType = Generics::_impl::swift_interop_passStub_Generics_void_ptr_0_8_void_ptr_8_16;
#elif UINTPTR_MAX == 0xFFFFFFFF
using stubType = Generics::_impl::swift_interop_passStub_Generics_void_ptr_0_4_void_ptr_4_8;
#endif

// verify that we're using direct stub type in the test.
static_assert(sizeof(stubType) == sizeof(void *) * 2);
