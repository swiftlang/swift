// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/generics.swift -module-name Generics -cxx-interoperability-mode=default -typecheck -verify -emit-clang-header-path %t/generics.h

// RUN: %target-interop-build-clangxx -fno-exceptions -std=gnu++20 -c %t/generic-hashable-execution.cpp -I %t -o %t/swift-generics-execution.o
// RUN: %target-build-swift %t/generics.swift -o %t/swift-generics-execution -Xlinker %t/swift-generics-execution.o -module-name Generics -Xfrontend -entry-point-function-name -Xfrontend swiftMain
// RUN: %target-codesign %t/swift-generics-execution
// RUN: %target-run %t/swift-generics-execution | %FileCheck %s

// RUN: %target-interop-build-clangxx -fno-exceptions -std=gnu++20 -c %t/non-hashable-execution.cpp -I %t -o %t/non-hashable-execution.o
// RUN: %target-build-swift %t/generics.swift -o %t/non-hashable-execution -Xlinker %t/non-hashable-execution.o -module-name Generics -Xfrontend -entry-point-function-name -Xfrontend swiftMain
// RUN: %target-codesign %t/non-hashable-execution
// RUN: not --crash %target-run %t/non-hashable-execution 2>&1 | %FileCheck %s --check-prefix=NON-HASHABLE

// REQUIRES: executable_test

//--- generics.swift
@_expose(Cxx)
public func genericAreEqual<T: Hashable>(_ a: T, _ b: T) -> Bool {
    return a == b
}

@_expose(Cxx)
public func printIsEqual<T: Hashable>(_ a: T, _ b: T) {
    print(a == b ? "EQUAL" : "NOT EQUAL")
}

@_expose(Cxx)
public func genericBothAreEqual<A: Hashable, B: Hashable>(
    _ a1: A, _ a2: A, _ b1: B, _ b2: B
) -> Bool {
    return a1 == a2 && b1 == b2
}

@_expose(Cxx)
public struct ConditionallyHashable<Value> {
    public let value: Value

    public init(_ value: Value) {
        self.value = value
    }
}

extension ConditionallyHashable: Equatable where Value: Equatable {}
extension ConditionallyHashable: Hashable where Value: Hashable {}

@_expose(Cxx)
public struct ConditionalExtensionHost<Value> {
    public let value: Value

    public init(_ value: Value) {
        self.value = value
    }
}

extension ConditionalExtensionHost where Value: Hashable {
    public func isSelfEqual() -> Bool {
        return value == value
    }
}

@_expose(Cxx)
public struct NonHashable {
    public let value: Int

    public init(_ value: Int) {
        self.value = value
    }
}

@_expose(Cxx)
public struct HashableMethodHost {
    private var marker: Int

    public init() {
        marker = 0
    }

    public func areEqual<Value: Hashable>(_ lhs: Value, _ rhs: Value) -> Bool {
        return lhs == rhs
    }

    public func overloadUnconstrainedFirst<Value>(_ value: Value) -> Int {
        return 1
    }

    public func overloadUnconstrainedFirst<Value: Hashable>(
        _ value: Value
    ) -> Int {
        return 2
    }

    public func overloadConstrainedFirst<Value: Hashable>(
        _ value: Value
    ) -> Int {
        return 2
    }

    public func overloadsWithDifferentSignatures<Value: Hashable>(
        _ value: Value
    ) -> Int {
        return 2
    }

    public func overloadsWithDifferentSignatures<Value>(
        _ value: Value, marker: Int
    ) -> Int {
        return 1
    }

    public func markerOverloadConstrainedFirst<Value: Sendable>(
        _ value: Value
    ) -> Int {
        return 2
    }

    public func markerOverloadConstrainedFirst<Value>(_ value: Value) -> Int {
        return 1
    }
}

extension HashableMethodHost {
    public func overloadConstrainedFirst<Value>(_ value: Value) -> Int {
        return 1
    }
}

@_expose(Cxx)
public struct UnconstrainedSubscriptFirst {
    private var marker: Int

    public init() {
        marker = 0
    }

    public subscript<Value>(_ value: Value) -> Int {
        return 1
    }

    public subscript<Value: Hashable>(hashable value: Value) -> Int {
        return 2
    }
}

@_expose(Cxx)
public struct ConstrainedSubscriptFirst {
    private var marker: Int

    public init() {
        marker = 0
    }

    public subscript<Value: Hashable>(_ value: Value) -> Int {
        return 2
    }

    public subscript<Value>(unconstrained value: Value) -> Int {
        return 1
    }
}

@_expose(Cxx)
public struct KeyedContainer<Key: Hashable> {
    var storage: Set<Key>

    public init() {
        storage = []
    }

    public mutating func insert(_ key: Key) {
        storage.insert(key)
    }

    public func contains(_ key: Key) -> Bool {
        return storage.contains(key)
    }

    public var count: Int {
        return storage.count
    }
}

@_expose(Cxx)
public enum HashableChoice<Value: Hashable> {
    case value(Value)
    case empty

    public func contains(_ candidate: Value) -> Bool {
        switch self {
        case .value(let value):
            return value == candidate
        case .empty:
            return false
        }
    }
}

//--- generic-hashable-execution.cpp
#include <cassert>
#include "generics.h"

int main() {
  {
    // A generic function with a Hashable requirement.
    assert(Generics::genericAreEqual(1, 1));
    assert(!Generics::genericAreEqual(1, 2));
    assert(Generics::genericAreEqual(swift::String("abc"), swift::String("abc")));
    assert(Generics::genericBothAreEqual(
        1, 1, swift::String("abc"), swift::String("abc")));
    Generics::printIsEqual(-11, -11);
    Generics::printIsEqual(4.0, 2.0);
  }
// CHECK: EQUAL
// CHECK-NEXT: NOT EQUAL
  {
    // Resolve a conditional conformance declared by the Swift producer module.
    auto lhs = Generics::ConditionallyHashable<int>::init(7);
    auto rhs = Generics::ConditionallyHashable<int>::init(7);
    assert(Generics::genericAreEqual(lhs, rhs));
  }
  {
    // A conditional extension can add a supported Hashable requirement to an
    // otherwise-unconstrained generic type.
    auto host = Generics::ConditionalExtensionHost<int>::init(7);
    assert(host.isSelfEqual());
  }
  {
    // A generic method with a Hashable requirement.
    auto host = Generics::HashableMethodHost::init();
    assert(host.areEqual(17, 17));
    assert(!host.areEqual(swift::String("left"), swift::String("right")));
    auto nonHashable = Generics::NonHashable::init(3);
    // Generic requirements are not part of the C++ signature. The
    // unconstrained overload must win in either source order, including when
    // it is declared in an extension.
    assert(host.overloadUnconstrainedFirst(nonHashable) == 1);
    assert(host.overloadConstrainedFirst(nonHashable) == 1);
    assert(host.overloadUnconstrainedFirst(3) == 1);
    assert(host.overloadConstrainedFirst(3) == 1);

    // Ordering a same-named overload set must not discard declarations whose
    // final C++ parameter lists do not collide.
    assert(host.overloadsWithDifferentSignatures(3) == 2);
    assert(host.overloadsWithDifferentSignatures(nonHashable, 0) == 1);

    // Marker-protocol requirements are also erased from the C++ signature,
    // even though they do not require a runtime witness lookup.
    assert(host.markerOverloadConstrainedFirst(3) == 1);
  }
  {
    // Subscript labels and generic requirements are not part of the C++
    // operator[] signature. The unconstrained overload must win in either
    // source order.
    auto value = Generics::NonHashable::init(3);
    auto unconstrainedFirst = Generics::UnconstrainedSubscriptFirst::init();
    auto constrainedFirst = Generics::ConstrainedSubscriptFirst::init();
    assert(unconstrainedFirst[value] == 1);
    assert(constrainedFirst[value] == 1);
  }
  {
    // A generic type with a Hashable requirement.
    auto container = Generics::KeyedContainer<int>::init();
    assert(container.getCount() == 0);
    container.insert(42);
    container.insert(42);
    container.insert(7);
    assert(container.getCount() == 2);
    assert(container.contains(42));
    assert(container.contains(7));
    assert(!container.contains(9));

    // Copy and destroy the value.
    auto copy = container;
    copy.insert(1);
    assert(copy.getCount() == 3);
    assert(container.getCount() == 2);
  }
  {
    auto container = Generics::KeyedContainer<swift::String>::init();
    container.insert("hello");
    assert(container.contains("hello"));
    assert(!container.contains("world"));
  }
  {
    // A generic enum with a Hashable requirement.
    auto choice = Generics::HashableChoice<int>::value(23);
    assert(choice.contains(23));
    assert(!choice.contains(42));
    auto empty = Generics::HashableChoice<int>::empty();
    assert(!empty.contains(23));
  }
  return 0;
}

//--- non-hashable-execution.cpp
#include "generics.h"

int main() {
  auto value = Generics::NonHashable::init(7);
  (void)Generics::genericAreEqual(value, value);
  return 0;
}

// NON-HASHABLE: Fatal error: Swift protocol conformance required by generic requirements is unavailable
