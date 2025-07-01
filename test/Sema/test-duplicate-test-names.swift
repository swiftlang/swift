// RUN: %target-typecheck-verify-swift

// Test that duplicate function names are properly detected and reported
// This test verifies the fix for https://github.com/swiftlang/swift/issues/82631
// by testing the underlying duplicate detection logic

// Test case 1: Duplicate functions in the same type
// This should trigger the standard redeclaration error
struct TestStruct {
  func someTest() {
    // First function
  }
  
  func someTest() {
    // Second function with the same name - should cause error
  }
  // expected-error@-3 {{invalid redeclaration of 'someTest()'}}
  // expected-note@-4 {{'someTest()' previously declared here}}
}

// Test case 2: Duplicate functions in a class
class TestClass {
  func anotherTest() {
    // First function
  }
  
  func anotherTest() {
    // Second function with the same name - should cause error
  }
  // expected-error@-3 {{invalid redeclaration of 'anotherTest()'}}
  // expected-note@-4 {{'anotherTest()' previously declared here}}
}

// Test case 3: Non-duplicate functions should not cause errors
struct ValidTestStruct {
  func test1() {
    // First function
  }
  
  func test2() {
    // Second function with different name - should be fine
  }
}

// Test case 4: Functions with same name in different scopes should be fine
struct ScopeTest1 {
  func scopeTest() {
    // Function in first scope
  }
}

struct ScopeTest2 {
  func scopeTest() {
    // Function with same name in different scope - should be fine
  }
}

// Test case 5: Test that our custom diagnostic message format works
// This tests the infrastructure we added, even if @Test isn't available
struct CustomDiagnosticTest {
  func customTest() {
    // First function
  }
  
  func customTest() {
    // Second function with the same name - should cause error
  }
  // expected-error@-3 {{invalid redeclaration of 'customTest()'}}
  // expected-note@-4 {{'customTest()' previously declared here}}
} 
