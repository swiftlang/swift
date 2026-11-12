// The generated C++ thunk retains a foreign reference before passing it to a
// Swift parameter that consumes it, so the reference count stays balanced.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t%{fs-sep}use-cxx-types.swift -module-name UseCxx -typecheck -emit-clang-header-path %t%{fs-sep}UseCxx.h -I %t -cxx-interoperability-mode=default -clang-header-expose-decls=all-public -target %target-swift-5.8-abi-triple
// RUN: %FileCheck %s --check-prefix=HEADER < %t%{fs-sep}UseCxx.h

// RUN: %target-interop-build-clangxx -std=c++20 -c %t%{fs-sep}use-swift-cxx-types.cpp -I %t -o %t%{fs-sep}swift-cxx-execution.o

// RUN: %target-interop-build-swift %t%{fs-sep}use-cxx-types.swift -o %t%{fs-sep}onone -Xlinker %t%{fs-sep}swift-cxx-execution.o -module-name UseCxx -Xfrontend -entry-point-function-name -Xfrontend swiftMain -I %t -Onone -target %target-swift-5.8-abi-triple
// RUN: %target-codesign %t%{fs-sep}onone
// RUN: %target-run %t%{fs-sep}onone | %FileCheck %s

// RUN: %target-interop-build-swift %t%{fs-sep}use-cxx-types.swift -o %t%{fs-sep}o -Xlinker %t%{fs-sep}swift-cxx-execution.o -module-name UseCxx -Xfrontend -entry-point-function-name -Xfrontend swiftMain -I %t -O -target %target-swift-5.8-abi-triple
// RUN: %target-codesign %t%{fs-sep}o
// RUN: %target-run %t%{fs-sep}o | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: stdlib_5_8_runtime

//--- header.h

#include <stdio.h>

class SharedFRT {
public:
  int refs = 1;
  int x = 0;
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainShared")))
__attribute__((swift_attr("release:releaseShared")));

inline void retainShared(SharedFRT *r) { ++r->refs; }
inline void releaseShared(SharedFRT *r) {
  if (--r->refs == 0)
    printf("released the last reference to x=%d\n", r->x);
}

// Inherits the annotation. Swift retains it through a forwarding method that
// the importer synthesizes, but C++ can call retainShared() directly.
class DerivedFRT : public SharedFRT {};

class MemberFRT {
public:
  int refs = 1;
  void retain() { ++refs; }
  void release() {
    if (--refs == 0)
      printf("released the last reference to a MemberFRT\n");
  }
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:.retain")))
__attribute__((swift_attr("release:.release")));

//--- module.modulemap
module CxxTest {
  header "header.h"
  requires cplusplus
}

//--- use-cxx-types.swift
import CxxTest

public func consumeShared(_ s: consuming SharedFRT) -> Int32 { s.x }
public func consumeOptionalShared(_ s: consuming SharedFRT?) -> Int32 {
  s?.x ?? -1
}
public func takeShared(_ s: SharedFRT) -> Int32 { s.x }
public func consumeDerived(_ d: consuming DerivedFRT) -> Int32 { d.x }
public func consumeMember(_ m: consuming MemberFRT) -> Int32 { m.refs }

public struct Holder {
  public var shared: SharedFRT
  public var optionalShared: SharedFRT?

  public init(_ shared: SharedFRT, _ optionalShared: SharedFRT?) {
    self.shared = shared
    self.optionalShared = optionalShared
  }
}

// HEADER-LABEL: SWIFT_INLINE_THUNK int32_t consumeOptionalShared(SharedFRT *_Nullable s) noexcept
// HEADER-NEXT:    SharedFRT *_Nullable consumedParamCopy_s = s;
// HEADER-NEXT:    if (consumedParamCopy_s)
// HEADER-NEXT:      ::retainShared(consumedParamCopy_s);
// HEADER-NEXT:    return UseCxx::_impl::$s6UseCxx21consumeOptionalShared{{[^(]*}}(consumedParamCopy_s);
// HEADER-NEXT:  }

// A parameter that Swift does not consume is passed as is.
// HEADER-LABEL: SWIFT_INLINE_THUNK int32_t takeShared(SharedFRT *_Nonnull s) noexcept
// HEADER-NEXT:    return UseCxx::_impl::$s6UseCxx10takeShared{{[^(]*}}(s);
// HEADER-NEXT:  }

//--- use-swift-cxx-types.cpp

#include "header.h"
#include "UseCxx.h"

int main() {
  SharedFRT s;
  s.x = 5;

  int consumed = UseCxx::consumeShared(&s);
  printf("consume=%d refs=%d\n", consumed, s.refs);
  // CHECK: consume=5 refs=1

  int consumedOptional = UseCxx::consumeOptionalShared(&s);
  printf("consumeOptional=%d refs=%d\n", consumedOptional, s.refs);
  // CHECK-NEXT: consumeOptional=5 refs=1

  int consumedNull = UseCxx::consumeOptionalShared(nullptr);
  printf("consumeNull=%d refs=%d\n", consumedNull, s.refs);
  // CHECK-NEXT: consumeNull=-1 refs=1

  int taken = UseCxx::takeShared(&s);
  printf("take=%d refs=%d\n", taken, s.refs);
  // CHECK-NEXT: take=5 refs=1

  DerivedFRT d;
  d.x = 7;
  int consumedDerived = UseCxx::consumeDerived(&d);
  printf("consumeDerived=%d refs=%d\n", consumedDerived, d.refs);
  // CHECK-NEXT: consumeDerived=7 refs=1

  MemberFRT m;
  int consumedMember = UseCxx::consumeMember(&m);
  printf("consumeMember=%d refs=%d\n", consumedMember, m.refs);
  // CHECK-NEXT: consumeMember={{[0-9]+}} refs=1

  {
    auto holder = UseCxx::Holder::init(&s, &s);
    printf("init refs=%d\n", s.refs);
    // CHECK-NEXT: init refs=3

    holder.setShared(&s);
    printf("set refs=%d\n", s.refs);
    // CHECK-NEXT: set refs=3

    holder.setOptionalShared(nullptr);
    printf("setOptionalNull refs=%d\n", s.refs);
    // CHECK-NEXT: setOptionalNull refs=2

    holder.setOptionalShared(&s);
    printf("setOptional refs=%d\n", s.refs);
    // CHECK-NEXT: setOptional refs=3
  }
  printf("end refs=%d\n", s.refs);
  // CHECK-NEXT: end refs=1
  return 0;
}
