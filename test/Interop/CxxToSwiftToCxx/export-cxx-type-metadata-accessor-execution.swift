// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck %t/use-cxx-types.swift %t/box.swift -module-name UseCxx \
// RUN:   -I %t -cxx-interoperability-mode=default -clang-header-expose-decls=all-public \
// RUN:   -emit-clang-header-path %t/UseCxx.h
// RUN: %target-interop-build-clangxx -std=c++20 -c %t/use-swift-cxx-types.cpp -I %t \
// RUN:   -o %t/use-swift-cxx-types.o

// The Swift module is a dynamic library. Its files are compiled separately, so
// it contains several copies of the metadata accessors.
// RUN: %target-build-swift-dylib(%t/%target-library-name(UseCxx)) %t/use-cxx-types.swift \
// RUN:   %t/box.swift -module-name UseCxx -I %t -cxx-interoperability-mode=default
// RUN: %target-codesign %t/%target-library-name(UseCxx)
// RUN: %target-interop-build-swift %t/main.swift %t/use-swift-cxx-types.o -o %t/dylib-client \
// RUN:   -L %t -lUseCxx %target-rpath(%t) \
// RUN:   -Xfrontend -entry-point-function-name -Xfrontend swiftMain
// RUN: %target-codesign %t/dylib-client
// RUN: %target-run %t/dylib-client %t/%target-library-name(UseCxx) | %FileCheck %s

// The Swift module was prelinked with `ld -r`, which turns hidden symbols into
// local ones on Darwin. SwiftPM does this for package targets.
// RUN: %if OS_FAMILY=darwin %{ \
// RUN:   %target-swift-frontend -c -primary-file %t/use-cxx-types.swift %t/box.swift \
// RUN:     -module-name UseCxx -I %t -cxx-interoperability-mode=default -parse-as-library \
// RUN:     -o %t/use-cxx-types.o && \
// RUN:   %target-swift-frontend -c %t/use-cxx-types.swift -primary-file %t/box.swift \
// RUN:     -module-name UseCxx -I %t -cxx-interoperability-mode=default -parse-as-library \
// RUN:     -o %t/box.o && \
// RUN:   %target-ld -r %t/use-cxx-types.o %t/box.o -o %t/UseCxx-prelinked.o && \
// RUN:   %target-interop-build-swift %t/main.swift %t/use-swift-cxx-types.o \
// RUN:     %t/UseCxx-prelinked.o -o %t/prelinked-client \
// RUN:     -Xfrontend -entry-point-function-name -Xfrontend swiftMain && \
// RUN:   %target-codesign %t/prelinked-client && \
// RUN:   %target-run %t/prelinked-client | %FileCheck %s \
// RUN: %}

// REQUIRES: executable_test

// Type metadata for foreign types is not yet supported on Windows. Bug #76168.
// UNSUPPORTED: OS=windows-msvc

//--- header.h

struct CStruct {
  int x;
};

struct CxxStruct {
  inline CxxStruct(int x) : x(x) {}
  inline CxxStruct(const CxxStruct &other) : x(other.x) {}
  inline ~CxxStruct() {}

  int x;
};

struct CxxStruct2 {
  inline CxxStruct2(int x) : x(x) {}
  inline CxxStruct2(const CxxStruct2 &other) : x(other.x) {}
  inline ~CxxStruct2() {}

  int x;
};

namespace ns {
struct Inner {
  int x;
};
} // namespace ns

struct ParamOnly {
  int x;
};

struct EnumPayload {
  int x;
};

//--- module.modulemap
module CxxTest {
  header "header.h"
  requires cplusplus
}

//--- use-cxx-types.swift
import CxxTest

public func makeCxxStruct(_ x: Int32) -> CxxStruct? {
  return CxxStruct(x)
}

public func unwrap(_ value: CxxStruct?) -> Int32 {
  return value?.x ?? -1
}

public func sum(_ values: [CxxStruct2]) -> Int32 {
  return values.reduce(0) { $0 + $1.x }
}

public func unwrapC(_ value: CStruct?) -> Int32 {
  return value?.x ?? -1
}

public func count(_ values: [ns.Inner]) -> Int {
  return values.count
}

// C++ can also use the types that the public API doesn't use as generic
// arguments in generic contexts.
public func takeParamOnly(_ value: ParamOnly) -> Int32 {
  return value.x
}

public enum WithPayload {
  case payload(EnumPayload)
}

public func genericCount<T>(_ values: [T]) -> Int {
  return values.count
}

//--- box.swift
import CxxTest

// Uses the accessor, but doesn't need it to be exported.
public func box(_ value: CxxStruct) -> Any {
  return value
}

//--- main.swift

//--- use-swift-cxx-types.cpp

#include "header.h"
#include "UseCxx.h"
#include <cstdio>

int main() {
  auto optional = swift::Optional<CxxStruct>::init(CxxStruct(42));
  std::printf("unwrap: %d\n", static_cast<int>(UseCxx::unwrap(optional)));
  std::printf("isSome: %d\n", UseCxx::makeCxxStruct(1).isSome());

  auto values = swift::Array<CxxStruct2>::init();
  values.append(CxxStruct2(1));
  values.append(CxxStruct2(2));
  std::printf("sum: %d\n", static_cast<int>(UseCxx::sum(values)));

  auto value = swift::Optional<CStruct>::init(CStruct{7});
  std::printf("unwrapC: %d\n", static_cast<int>(UseCxx::unwrapC(value)));

  auto inner = swift::Array<ns::Inner>::init();
  inner.append(ns::Inner{3});
  std::printf("count: %d\n", static_cast<int>(UseCxx::count(inner)));

  auto paramOnly = swift::Array<ParamOnly>::init();
  paramOnly.append(ParamOnly{4});
  std::printf("genericCount: %d\n",
              static_cast<int>(UseCxx::genericCount(paramOnly)));

  auto payloads = swift::Array<EnumPayload>::init();
  payloads.append(EnumPayload{5});
  payloads.append(EnumPayload{6});
  std::printf("genericCount: %d\n",
              static_cast<int>(UseCxx::genericCount(payloads)));
  return 0;
}

// CHECK: unwrap: 42
// CHECK-NEXT: isSome: 1
// CHECK-NEXT: sum: 3
// CHECK-NEXT: unwrapC: 7
// CHECK-NEXT: count: 1
// CHECK-NEXT: genericCount: 1
// CHECK-NEXT: genericCount: 2
