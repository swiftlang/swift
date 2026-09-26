// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t%{fs-sep}use-cxx-types.swift -module-name UseCxx -typecheck -emit-clang-header-path %t%{fs-sep}UseCxx.h -I %t -cxx-interoperability-mode=default -clang-header-expose-decls=all-public -disable-availability-checking -enable-experimental-feature GenerateBindingsForHashableRequirementsInCXX

// RUN: %target-interop-build-clangxx -std=c++20 -c %t%{fs-sep}use-swift-cxx-types.cpp -I %t -o %t%{fs-sep}swift-cxx-execution.o
// RUN: %target-interop-build-swift %t%{fs-sep}use-cxx-types.swift -o %t%{fs-sep}swift-cxx-execution -Xlinker %t%{fs-sep}swift-cxx-execution.o -module-name UseCxx -Xfrontend -entry-point-function-name -Xfrontend swiftMain -I %t -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -enable-experimental-feature GenerateBindingsForHashableRequirementsInCXX

// RUN: %target-codesign %t%{fs-sep}swift-cxx-execution
// RUN: %target-run %t%{fs-sep}swift-cxx-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_GenerateBindingsForHashableRequirementsInCXX

// A C++ type whose Hashable conformance is declared in Swift can be passed to
// a Swift API with a Hashable requirement from C++, and be used as a
// swift::Dictionary key.

//--- header.h

struct Key {
  int id;

  inline Key(int id) : id(id) {}
};

struct SharedKey {
  int id;

  inline SharedKey(int id) : id(id) {}
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")));

//--- module.modulemap
module CxxTest {
  header "header.h"
  requires cplusplus
}

//--- use-cxx-types.swift
import CxxTest

extension Key: @retroactive Hashable {
  public static func == (lhs: Key, rhs: Key) -> Bool { lhs.id == rhs.id }
  public func hash(into hasher: inout Hasher) { hasher.combine(id) }
}

extension SharedKey: @retroactive Hashable {
  public static func == (lhs: SharedKey, rhs: SharedKey) -> Bool {
    lhs.id == rhs.id
  }
  public func hash(into hasher: inout Hasher) { hasher.combine(id) }
}

public func areEqual<T: Hashable>(_ a: T, _ b: T) -> Bool {
  return a == b
}

public func printKey(_ key: Key) {
  print("Key(\(key.id))")
}

public func makeDictionary() -> [Key: CInt] {
  return [Key(1): 10, Key(2): 20]
}

public func printDictionary(_ dict: [Key: CInt]) {
  for key in dict.keys.sorted(by: { $0.id < $1.id }) {
    print("Key(\(key.id))=\(dict[key]!)")
  }
}

public func printSharedKey(_ key: SharedKey) {
  print("SharedKey(\(key.id))")
}

//--- use-swift-cxx-types.cpp
#include "header.h"
#include "UseCxx.h"
#include <assert.h>

int main() {
  {
    Key one(1);
    assert(UseCxx::areEqual(one, Key(1)));
    assert(!UseCxx::areEqual(one, Key(2)));
    UseCxx::printKey(one);
  }
// CHECK: Key(1)
  {
    auto dict = UseCxx::makeDictionary();
    assert(dict[Key(1)].get() == 10);
    assert(!dict[Key(3)]);
    dict.updateValueForKey(30, Key(3));
    UseCxx::printDictionary(dict);

    auto newDict = swift::Dictionary<Key, int>::init();
    newDict.updateValueForKey(40, Key(4));
    assert(newDict[Key(4)].get() == 40);
    UseCxx::printDictionary(newDict);
  }
// CHECK-NEXT: Key(1)=10
// CHECK-NEXT: Key(2)=20
// CHECK-NEXT: Key(3)=30
// CHECK-NEXT: Key(4)=40
  {
    // A foreign reference type is passed as a pointer.
    SharedKey *one = new SharedKey(1);
    SharedKey *otherOne = new SharedKey(1);
    SharedKey *two = new SharedKey(2);
    assert(UseCxx::areEqual(one, otherOne));
    assert(!UseCxx::areEqual(one, two));
    UseCxx::printSharedKey(one);
  }
// CHECK-NEXT: SharedKey(1)
  return 0;
}
