// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/use-dictionary.swift -module-name UseDictionary -cxx-interoperability-mode=default -typecheck -verify -emit-clang-header-path %t/UseDictionary.h

// RUN: %target-interop-build-clangxx -fno-exceptions -std=gnu++20 -c %t/dictionary-memory-execution.cpp -I %t -o %t/swift-stdlib-execution.o
// RUN: %target-build-swift %t/use-dictionary.swift -o %t/swift-stdlib-execution -Xlinker %t/swift-stdlib-execution.o -module-name UseDictionary -Xfrontend -entry-point-function-name -Xfrontend swiftMain
// RUN: %target-codesign %t/swift-stdlib-execution
// RUN: %target-run %t/swift-stdlib-execution | %FileCheck %s

// REQUIRES: executable_test

//--- use-dictionary.swift
public final class Tracked {
    public let id: Int
    public init(id: Int) {
        self.id = id
        Counter.live += 1
    }
    deinit { Counter.live -= 1 }
}

public enum Counter {
    public static var live = 0
}

@_expose(Cxx)
public func makeTracked(_ id: Int) -> Tracked {
    return Tracked(id: id)
}

@_expose(Cxx)
public func liveCount() -> Int {
    return Counter.live
}

@_expose(Cxx)
public func printLiveCount() {
    print("live=\(Counter.live)")
}

//--- dictionary-memory-execution.cpp
#include <cassert>
#include "UseDictionary.h"

// Verifies that the value witness table operations used by the generated
// swift::Dictionary bindings (which pass a 'Key: Hashable' witness table to
// the type metadata accessor) manage the lifetime of the values correctly.
int main() {
  using namespace swift;
  {
    auto dict = Dictionary<int, UseDictionary::Tracked>::init();
    dict.updateValueForKey(UseDictionary::makeTracked(1), 10);
    dict.updateValueForKey(UseDictionary::makeTracked(2), 20);
    assert(UseDictionary::liveCount() == 2);
    UseDictionary::printLiveCount();
// CHECK: live=2
    {
      // Copying the dictionary creates or destroys no Tracked instances.
      auto copy = dict;
      assert(UseDictionary::liveCount() == 2);
      copy.updateValueForKey(UseDictionary::makeTracked(3), 30);
      assert(UseDictionary::liveCount() == 3);
      UseDictionary::printLiveCount();
// CHECK-NEXT: live=3
    }
    // Destroying the copy destroys its newly inserted instance while the
    // instances shared with the original dictionary remain alive.
    assert(UseDictionary::liveCount() == 2);
    UseDictionary::printLiveCount();
// CHECK-NEXT: live=2
    auto removed = dict.removeValueForKey(10);
    assert(removed);
    // The removed value is still owned by the returned optional.
    assert(UseDictionary::liveCount() == 2);
  }
  // Destroying the dictionary and the optional releases everything.
  UseDictionary::printLiveCount();
// CHECK-NEXT: live=0
  assert(UseDictionary::liveCount() == 0);
  return 0;
}
