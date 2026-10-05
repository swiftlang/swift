// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: not %target-swift-frontend -typecheck -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop -diagnostic-style llvm 2>&1 | %FileCheck %s

//--- Inputs/module.modulemap
module Test {
    header "test.h"
    requires cplusplus
}

//--- Inputs/test.h
struct Ptr { int *p; };
struct __attribute__((swift_attr("import_owned"))) StringLiteral { const char *name; };

struct M {
  M(const M&);
  int *_Nonnull test1() const;
  int &test2() const;
  Ptr test3() const;

  int *begin() const;

  StringLiteral stringLiteral() const { return StringLiteral{"M"}; }
};

struct HasNonIteratorBeginMethod {
  void begin() const;
  void end() const;
};

//--- test.swift

import Test

public func test(x: M) {
  // CHECK: error: expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'
  // CHECK: note: reference to unsafe instance method 'test1()'
  // CHECK: note: this returns a pointer or reference into a type that owns its storage
  x.test1()
  // CHECK: error: expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'
  // CHECK: note: reference to unsafe instance method 'test2()'
  // CHECK: note: this returns a pointer or reference into a type that owns its storage
  x.test2()
  // CHECK: error: expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'
  // CHECK: note: reference to unsafe instance method 'test3()'
  // CHECK: note: this returns a view into a type that owns its storage
  x.test3()
  // CHECK: error: expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'
  // CHECK: note: reference to unsafe instance method 'begin()'
  // CHECK: note: 'begin' and 'end' are assumed to return iterators, which do not keep the underlying storage alive
  x.begin()

  // CHECK-NOT: error: value of type 'M' has no member 'stringLiteral'
  x.stringLiteral()
}

public func test(_ x: HasNonIteratorBeginMethod) {
  x.begin()
  x.end()
}
