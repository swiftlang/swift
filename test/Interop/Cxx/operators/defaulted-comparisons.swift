// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -cxx-interoperability-mode=default -Xcc -std=c++20)

// REQUIRES: executable_test
// REQUIRES: std_compare

import DefaultedComparisons
import StdlibUnittest

var OperatorsTestSuite = TestSuite("DefaultedComparisons")

OperatorsTestSuite.test("SingleField.== (single field)") {
    let a = SingleField(42)
    let b = SingleField(42)
    let c = SingleField(123)

    expectTrue(a == b)
    expectFalse(a == c)
}

OperatorsTestSuite.test("MultipleFields.== (multiple fields)") {
    let a = MultipleFields(1, 2)
    let b = MultipleFields(1, 2)
    let c = MultipleFields(1, 3)
    let d = MultipleFields(2, 2)

    expectTrue(a == b)
    expectFalse(a == c)
    expectFalse(a == d)
}

OperatorsTestSuite.test("Outer.== (transitive defaulted inner)") {
    let a = Outer(1)
    let b = Outer(1)
    let c = Outer(2)

    expectTrue(a == b)
    expectFalse(a == c)
}

OperatorsTestSuite.test("OuterExplicit.== (explicit outer, defaulted inner)") {
    let a = OuterExplicit(1)
    let b = OuterExplicit(1)
    let c = OuterExplicit(2)

    expectTrue(a == b)
    expectFalse(a == c)
}

OperatorsTestSuite.test("FriendEq.== (friend operator)") {
    let a = FriendEq(42)
    let b = FriendEq(42)
    let c = FriendEq(123)

    expectTrue(a == b)
    expectFalse(a == c)
}

OperatorsTestSuite.test("WithNotEqual.!= (defaulted != with defaulted ==)") {
    let a = WithNotEqual(42)
    let b = WithNotEqual(42)
    let c = WithNotEqual(123)

    expectFalse(a != b)
    expectTrue(a != c)
}

OperatorsTestSuite.test("Spaceship.== (implicit == from <=>)") {
    let a = Spaceship(42)
    let b = Spaceship(42)
    let c = Spaceship(123)

    expectTrue(a == b)
    expectFalse(a == c)
}

OperatorsTestSuite.test("Derived.== (defaulted with inheritance)") {
    let a = Derived(1, 2)
    let b = Derived(1, 2)
    let c = Derived(1, 3)
    let d = Derived(2, 2)

    expectTrue(a == b)
    expectFalse(a == c)
    expectFalse(a == d)
}

OperatorsTestSuite.test("IntWrapper.== (defaulted in class template)") {
    let a = IntWrapper(42)
    let b = IntWrapper(42)
    let c = IntWrapper(123)

    expectTrue(a == b)
    expectFalse(a == c)
}

runAllTests()
