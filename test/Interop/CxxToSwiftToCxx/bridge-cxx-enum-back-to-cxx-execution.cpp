// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -parse-as-library %platform-module-dir/Swift.swiftmodule/%module-target-triple.swiftinterface -enable-library-evolution -disable-objc-attr-requires-foundation-module -typecheck -module-name Swift -parse-stdlib -enable-experimental-cxx-interop -clang-header-expose-decls=has-expose-attr -emit-clang-header-path %t/Swift.h  -experimental-skip-all-function-bodies -enable-experimental-feature LifetimeDependence
// RUN: %target-swift-frontend -typecheck %t/use-cxx-types.swift -typecheck -module-name UseCxx -emit-clang-header-path %t/UseCxx.h -I %t -enable-experimental-cxx-interop -clang-header-expose-decls=all-public

// RUN: %target-interop-build-clangxx -std=c++20 -c %t/use-swift-cxx-types.cpp -I %t -o %t/swift-cxx-execution.o -g
// RUN: %target-interop-build-swift %t/use-cxx-types.swift -o %t/swift-cxx-execution -Xlinker %t/swift-cxx-execution.o -module-name UseCxx -Xfrontend -entry-point-function-name -Xfrontend swiftMain -I %t -g

// RUN: %target-codesign %t/swift-cxx-execution
// RUN: %target-run %t/swift-cxx-execution

// REQUIRES: executable_test
// REQUIRES: swift_feature_LifetimeDependence

//--- header.h
enum class SomeEnum {
  first,
  second
};

//--- module.modulemap
module CxxTest {
    header "header.h"
    requires cplusplus
}

//--- use-cxx-types.swift
import CxxTest

public class SomethingSwift {
  public var someEnum: SomeEnum { get { return .first } }
  public init() {}
}

//--- use-swift-cxx-types.cpp

#include "header.h"
#include "Swift.h"
#include "UseCxx.h"
#include <assert.h>

class SomethingCxx {
public:
  SomethingCxx(UseCxx::SomethingSwift swiftPart): _swiftPart(swiftPart) { }

  SomeEnum getSomeEnum() { return _swiftPart.getSomeEnum(); }

private:
  UseCxx::SomethingSwift _swiftPart;
};

int main() {
  auto sp = UseCxx::SomethingSwift::init();
  auto sc = SomethingCxx(sp);
  auto e = sc.getSomeEnum();
  assert(e == SomeEnum::first);
  return 0;
}
