// RUN: %swift -i %s --args ASSERT_eq_Int 2>&1 | FileCheck %s -check-prefix=ASSERT_EQ_INT
// RUN: %swift -i %s --args ASSERT_eq_String 2>&1 | FileCheck %s -check-prefix=ASSERT_EQ_STRING

func test_ASSERT_eq_Int() {
  ASSERT_eq(2, 1 + 1, "should not fail")
  ASSERT_eq(42, 0, "should fail")
  // ASSERT_EQ_INT: Unit test ASSERT failed: values should be equal: should fail
  // ASSERT_EQ_INT: File {{.*}}UnitTest.swift, line [[@LINE-2]]
  // ASSERT_EQ_INT: Expected: 42
  // ASSERT_EQ_INT: Actual: 0
}

if (CommandLineArguments.arguments[0] == "ASSERT_eq_Int") {
  unit_test_init()
  test_ASSERT_eq_Int()
  unit_test_fini()
}

func test_ASSERT_eq_String() {
  ASSERT_eq("aaabbb", "aaa" + "bbb", "should not fail")
  ASSERT_eq("aaa", "bbb", "should fail")
  // ASSERT_EQ_STRING: Unit test ASSERT failed: values should be equal: should fail
  // ASSERT_EQ_STRING: File {{.*}}UnitTest.swift, line [[@LINE-2]]
  // ASSERT_EQ_STRING: Expected: aaa
  // ASSERT_EQ_STRING: Actual: bbb
}

if (CommandLineArguments.arguments[0] == "ASSERT_eq_String") {
  test_ASSERT_eq_String()
}

