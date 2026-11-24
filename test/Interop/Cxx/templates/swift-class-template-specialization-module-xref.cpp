// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swiftxx-frontend -emit-module %t/Inputs/test.swift -module-name TestA -I %t/Inputs -o %t/test-part.swiftmodule
// RUN: %target-swiftxx-frontend -merge-modules -emit-module %t/test-part.swiftmodule -module-name TestA -I %t/Inputs -o %t/TestA.swiftmodule -sil-verify-none
// RUN: %target-swift-ide-test -print-module -module-to-print=TestA -I %t/ -source-filename=test -enable-experimental-cxx-interop | %FileCheck %s
// RUN: %target-swiftxx-frontend -emit-sil -O %t/Inputs/client.swift -I %t -I %t/Inputs -o /dev/null

//--- Inputs/module.modulemap
module CxxHeader {
    header "header.h"
    requires cplusplus
}

//--- Inputs/header.h

#include <stddef.h>

namespace std2 {

template<class T>
class vec {
public:
  using Element = T;
  using RawIterator = const T  * _Nonnull;
  vec() {}
  vec(const vec<T> &other) : items(other.items) { }
  ~vec() {}

  T * _Nonnull begin() {
    return items;
  }
  T * _Nonnull end() {
    return items + 10;
  }
  RawIterator begin() const {
    return items;
  }
  RawIterator end() const {
    return items + 10;
  }
  size_t size() const {
    return 10;
  }

private:
  T items[10];
};

} // namespace std2

namespace ns2 {

class App {
public:

  inline std2::vec<App> getApps() const {
    return {};
  }
  int x  = 0;
};

} // namespace ns2

using vec2Apps = std2::vec<ns2::App>;

template <class T>
struct TopLevelBox {
  T value;
};

using TopLevelBoxInt = TopLevelBox<int>;
using TopLevelBoxBool = TopLevelBox<bool>;

// A macro with the same name as the template is also found by module lookup.
#define TopLevelBox 42

//--- Inputs/test.swift

import Cxx
import CxxHeader

extension vec2Apps : CxxSequence {
}

public func testFunction() -> [Int] {
  let applications = ns2.App().getApps()
  return applications.map { Int($0.x) }
}

public func makeTopLevelBox() -> TopLevelBoxInt {
  return TopLevelBoxInt(value: 42)
}

extension TopLevelBoxBool {
  public func isSet() -> Bool { return value }
}

@inlinable public func inlinableTopLevelBox() -> CInt {
  let b = TopLevelBoxInt(value: 42)
  return b.value
}

//--- Inputs/client.swift

import TestA

public func client() -> CInt {
  return inlinableTopLevelBox()
}

// CHECK: @inlinable func inlinableTopLevelBox() -> CInt
// CHECK: func makeTopLevelBox() -> TopLevelBoxInt
// CHECK: func testFunction() -> [Int]
// CHECK: extension std2.vec<ns2.App> : CxxSequence {
// CHECK: extension TopLevelBox<CBool> {
// CHECK-NEXT:   func isSet() -> Bool
