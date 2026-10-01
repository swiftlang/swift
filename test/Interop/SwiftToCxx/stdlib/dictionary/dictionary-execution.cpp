// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/use-dictionary.swift -module-name UseDictionary -cxx-interoperability-mode=default -enable-experimental-feature GenerateBindingsForHashableRequirementsInCXX -typecheck -verify -emit-clang-header-path %t/UseDictionary.h

// RUN: %target-interop-build-clangxx -fno-exceptions -std=gnu++20 -c %t/dictionary-execution.cpp -I %t -o %t/swift-stdlib-execution.o
// RUN: %target-interop-build-swift %t/use-dictionary.swift -o %t/swift-stdlib-execution -Xlinker %t/swift-stdlib-execution.o -module-name UseDictionary -Xfrontend -entry-point-function-name -Xfrontend swiftMain -enable-experimental-feature GenerateBindingsForHashableRequirementsInCXX
// RUN: %target-codesign %t/swift-stdlib-execution
// RUN: %target-run %t/swift-stdlib-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_GenerateBindingsForHashableRequirementsInCXX

//--- use-dictionary.swift
@_expose(Cxx)
public func createDictionary(_ key: CInt, _ value: CInt) -> [CInt: CInt] {
    return [key: value]
}

@_expose(Cxx)
public func passthroughDictionary(_ dict: [CInt: CInt]) -> Dictionary<CInt, CInt> {
    return dict
}

@_expose(Cxx)
public struct CustomKey: Hashable {
    public let rawValue: CInt

    public init(_ rawValue: CInt) {
        self.rawValue = rawValue
    }
}

@_expose(Cxx)
public func printDictionary(_ dict: Dictionary<CInt, CInt>) {
    var res = ""
    for key in dict.keys.sorted() {
        res += "\(key)=\(dict[key]!);"
    }
    print(res)
}

public func printStringDictionary(_ dict: [String: String]) {
    for key in dict.keys.sorted() {
        print("GOT '\(key)' -> '\(dict[key]!)'")
    }
    print("DONE PRINTING.")
}

@_expose(Cxx)
public class ClassKey: Hashable {
    public let id: CInt

    public init(_ id: CInt) {
        self.id = id
    }

    public static func == (lhs: ClassKey, rhs: ClassKey) -> Bool {
        return lhs.id == rhs.id
    }

    public func hash(into hasher: inout Hasher) {
        hasher.combine(id)
    }
}

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

//--- dictionary-execution.cpp
#include <cassert>
#include "UseDictionary.h"

int main() {
  using namespace swift;
  {
    // Read a Swift-created dictionary from C++.
    auto dict = UseDictionary::createDictionary(11, 42);
    assert(dict.getCount() == 1);
    assert(!dict.isEmpty());
    auto value = dict[11];
    assert(value);
    assert(value.get() == 42);
    auto missing = dict[7];
    assert(!missing);
    UseDictionary::printDictionary(UseDictionary::passthroughDictionary(dict));
  }
// CHECK: 11=42;
  {
    // Create and mutate a dictionary in C++, read it from Swift.
    auto dict = Dictionary<int, int>::init();
    assert(dict.getCount() == 0);
    assert(dict.isEmpty());
    auto old = dict.updateValueForKey(100, 1);
    assert(!old);
    old = dict.updateValueForKey(200, 1);
    assert(old);
    assert(old.get() == 100);
    dict.updateValueForKey(300, 3);
    assert(dict.getCount() == 2);
    UseDictionary::printDictionary(dict);
// CHECK-NEXT: 1=200;3=300;
    auto removed = dict.removeValueForKey(3);
    assert(removed);
    assert(removed.get() == 300);
    assert(dict.getCount() == 1);
    auto notRemoved = dict.removeValueForKey(99);
    assert(!notRemoved);
    UseDictionary::printDictionary(dict);
// CHECK-NEXT: 1=200;
  }
  {
    // Pass a dictionary value around by copy.
    auto dict = Dictionary<int, int>::init();
    dict.updateValueForKey(-1, 5);
    auto copy = dict;
    copy.updateValueForKey(-2, 6);
    assert(dict.getCount() == 1);
    assert(copy.getCount() == 2);
    assert(copy[5].get() == -1);
    assert(copy[6].get() == -2);
    UseDictionary::printDictionary(UseDictionary::passthroughDictionary(copy));
  }
// CHECK-NEXT: 5=-1;6=-2;
  {
    // Use Swift String keys and values from C++.
    auto dict = Dictionary<swift::String, swift::String>::init();
    dict.updateValueForKey("world", "hello");
    dict.updateValueForKey("swift", "hola");
    auto value = dict["hello"];
    assert(value);
    UseDictionary::printStringDictionary(dict);
  }
// CHECK-NEXT: GOT 'hello' -> 'world'
// CHECK-NEXT: GOT 'hola' -> 'swift'
// CHECK-NEXT: DONE PRINTING.
  {
    // The key need not have a C++ std::hash specialization. Dictionary uses
    // the Swift Hashable conformance associated with the key's type metadata.
    auto key = UseDictionary::CustomKey::init(8);
    auto dict = Dictionary<UseDictionary::CustomKey, int>::init();
    dict.updateValueForKey(99, key);
    auto value = dict[key];
    assert(value);
    assert(value.get() == 99);
  }
  {
    // A Swift class as the key.
    auto dict = Dictionary<UseDictionary::ClassKey, int>::init();
    dict.updateValueForKey(5, UseDictionary::ClassKey::init(1));
    auto value = dict[UseDictionary::ClassKey::init(1)];
    assert(value);
    assert(value.get() == 5);
    assert(!dict[UseDictionary::ClassKey::init(2)]);
  }
  {
    // The value witness table operations used by swift::Dictionary, whose type
    // metadata accessor takes the witness table for 'Key: Hashable', manage
    // the lifetime of the values correctly.
    auto dict = Dictionary<int, UseDictionary::Tracked>::init();
    dict.updateValueForKey(UseDictionary::makeTracked(1), 10);
    dict.updateValueForKey(UseDictionary::makeTracked(2), 20);
    assert(UseDictionary::liveCount() == 2);
    UseDictionary::printLiveCount();
// CHECK-NEXT: live=2
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
