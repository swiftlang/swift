// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// The TBD lists the metadata accessors that the generated header calls as weak
// definitions, like IRGen exports them.
// RUN: %target-swift-frontend -emit-ir %t/use-cxx-types.swift -module-name UseCxx -I %t \
// RUN:   -cxx-interoperability-mode=default -validate-tbd-against-ir=all \
// RUN:   -emit-tbd-path %t/UseCxx.tbd -tbd-install_name @rpath/libUseCxx.dylib -o %t/UseCxx.ll
// RUN: %FileCheck %s < %t/UseCxx.tbd
// RUN: %target-swift-frontend -emit-ir %t/resilient.swift -module-name Resilient -I %t \
// RUN:   -cxx-interoperability-mode=default -enable-library-evolution \
// RUN:   -validate-tbd-against-ir=all -emit-tbd-path %t/Resilient.tbd \
// RUN:   -tbd-install_name @rpath/libResilient.dylib -o %t/Resilient.ll
// RUN: %FileCheck %s --check-prefix=RESILIENT < %t/Resilient.tbd

// C++ code that links against the TBD runs with the actual library.
// RUN: %target-swift-frontend -typecheck %t/use-cxx-types.swift -module-name UseCxx -I %t \
// RUN:   -cxx-interoperability-mode=default -clang-header-expose-decls=all-public \
// RUN:   -emit-clang-header-path %t/UseCxx.h
// RUN: %target-interop-build-clangxx -std=c++20 -c %t/use-swift-cxx-types.cpp -I %t \
// RUN:   -o %t/use-swift-cxx-types.o
// RUN: %empty-directory(%t/stub)
// RUN: cp %t/UseCxx.tbd %t/stub/libUseCxx.tbd
// RUN: %target-interop-build-swift %t/main.swift %t/use-swift-cxx-types.o -o %t/client \
// RUN:   -L %t/stub -lUseCxx %target-rpath(%t) \
// RUN:   -Xfrontend -entry-point-function-name -Xfrontend swiftMain
// RUN: %target-build-swift-dylib(%t/libUseCxx.dylib) %t/use-cxx-types.swift -module-name UseCxx \
// RUN:   -I %t -cxx-interoperability-mode=default \
// RUN:   -Xlinker -install_name -Xlinker @rpath/libUseCxx.dylib
// RUN: %target-codesign %t/client %t/libUseCxx.dylib
// RUN: %target-run %t/client %t/libUseCxx.dylib | %FileCheck %s --check-prefix=OUTPUT

// REQUIRES: VENDOR=apple
// REQUIRES: executable_test

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

struct EnumPayload {
  int x;
};

struct MethodParam {
  int x;
};

//--- module.modulemap
module CxxTest {
  header "header.h"
  requires cplusplus
}

//--- use-cxx-types.swift
import CxxTest

public func unwrap(_ value: CStruct?) -> Int32 {
  return value?.x ?? -1
}

public func takeCxxStruct(_ value: CxxStruct) -> Int32 {
  return value.x
}

public enum WithPayload {
  case payload(EnumPayload)
}

open class Base {
  public init() {}
  open func method(_ x: MethodParam) {}
}

public func genericCount<T>(_ values: [T]) -> Int {
  return values.count
}

// CHECK:      "weak": [
// CHECK-NEXT:   "_$sSo11EnumPayloadVMa",
// CHECK-NEXT:   "_$sSo11MethodParamVMa",
// CHECK-NEXT:   "_$sSo7CStructVMa",
// CHECK-NEXT:   "_$sSo9CxxStructVMa"
// CHECK-NEXT: ]

//--- resilient.swift
import CxxTest

public func unwrap(_ value: CStruct?) -> Int32 {
  return value?.x ?? -1
}

// Only the dispatch thunk of the method is exported.
open class Base {
  public init() {}
  open func method(_ x: MethodParam) {}
}

// RESILIENT:      "weak": [
// RESILIENT-NEXT:   "_$sSo11MethodParamVMa",
// RESILIENT-NEXT:   "_$sSo7CStructVMa"
// RESILIENT-NEXT: ]

//--- main.swift

//--- use-swift-cxx-types.cpp

#include "header.h"
#include "UseCxx.h"
#include <cstdio>

int main() {
  auto value = swift::Optional<CStruct>::init(CStruct{7});
  std::printf("unwrap: %d\n", static_cast<int>(UseCxx::unwrap(value)));

  auto values = swift::Array<CxxStruct>::init();
  values.append(CxxStruct(1));
  values.append(CxxStruct(2));
  std::printf("genericCount: %d\n",
              static_cast<int>(UseCxx::genericCount(values)));
  return 0;
}

// OUTPUT: unwrap: 7
// OUTPUT-NEXT: genericCount: 2
