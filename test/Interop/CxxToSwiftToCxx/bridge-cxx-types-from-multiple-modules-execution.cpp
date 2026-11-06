// RUN: %empty-directory(%t)
// RUN: split-file %S/bridge-cxx-types-from-multiple-modules.swift %t

// RUN: %target-swift-frontend %t/first.swift -module-name First -emit-module -emit-module-path %t/First.swiftmodule -emit-clang-header-path %t/First.h -I %t -cxx-interoperability-mode=default -clang-header-expose-decls=all-public
// RUN: %target-swift-frontend %t/second.swift -module-name Second -typecheck -emit-clang-header-path %t/Second.h -I %t -cxx-interoperability-mode=default -clang-header-expose-decls=all-public
// RUN: %target-swift-frontend %t/third.swift -module-name Third -typecheck -emit-clang-header-path %t/Third.h -I %t -cxx-interoperability-mode=default -clang-header-expose-decls=all-public -clang-header-expose-module First=First.h

// RUN: %target-interop-build-clangxx -std=c++17 -c %s -I %t -o %t/main.o
// RUN: %target-interop-build-swift %t/first.swift -module-name First -parse-as-library -emit-object -o %t/first.o -I %t
// RUN: %target-interop-build-swift %t/second.swift -module-name Second -parse-as-library -emit-object -o %t/second.o -I %t
// RUN: %target-interop-build-swift %t/third.swift -module-name Third -o %t/main -I %t -Xlinker %t/first.o -Xlinker %t/second.o -Xlinker %t/main.o -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test

#include "header.h"
#include "First.h"
#include "Second.h"
#include "Third.h"
#include <cstdio>

int main() {
  auto some = swift::Optional<CxxStruct>::init(CxxStruct{42});
  auto none = swift::Optional<CxxStruct>::none();
  printf("%d %d\n", int(First::firstStruct(some)), int(First::firstStruct(none)));
  // CHECK: 42 -1

  auto array = swift::Array<CxxStruct>::init();
  array.append(CxxStruct{1});
  array.append(CxxStruct{2});
  printf("%d\n", int(Second::secondStruct(array)));
  // CHECK-NEXT: 3

  auto b = swift::Optional<CxxEnum>::init(CxxEnum::b);
  printf("%d %d\n", int(First::firstEnum(b)), int(Second::secondEnum(b)));
  // CHECK-NEXT: 1 1

  auto t = swift::Optional<CxxTemplateInt>::init(CxxTemplateInt{7});
  printf("%d %d\n", int(First::firstTemplate(t)), int(Second::secondTemplate(t)));
  // CHECK-NEXT: 7 7

  auto d = swift::Optional<Cxx$Struct>::init(Cxx$Struct{5});
  printf("%d %d\n", int(First::firstDollar(d)), int(Second::secondDollar(d)));
  // CHECK-NEXT: 5 5

  printf("%d\n", int(Third::thirdBox(some).getValue()));
  // CHECK-NEXT: 43
  return 0;
}
