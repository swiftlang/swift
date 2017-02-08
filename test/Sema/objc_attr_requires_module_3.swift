// RUN: %target-build-swift -typecheck %s -Xfrontend -verify

class TestClass1 {
#if false
  @objc func testMethod() {} // OK, in inactive block
#endif
}

#if false
class TestClass2 {
  @objc func testMethod() {} // OK, in inactive block
}
#endif

#if false
class TestClass3 {
#if true
  @objc func testMethod() {} // OK, in inactive block
#endif
}
#endif

#if false
#if true
class TestClass4 {
  @objc func testMethod() {} // OK, in inactive block
}
#endif
#endif

class TestClass5 {
#if true
  // pass
#else
  @objc func testMethod() {} // OK, in inactive block
#endif
}

#if true
class TestClass6 {
  @objc func testMethod() {} // expected-error {{@objc attribute used without importing module 'Foundation'}}
}
#endif
