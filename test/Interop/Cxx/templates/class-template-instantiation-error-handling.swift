// RUN: not %target-swift-emit-sil %s -I %S/Inputs -enable-cxx-interop 2>&1 | %FileCheck %s

import ClassTemplateInstantiationErrors
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

// CHECK: class-template-instantiation-error-handling.swift:10:18: error: only C++ types supported
TemplatesTestSuite.test("swift-template-arg-not-supported") {
  var magicString = MagicWrapper<String>(t: "asdf")
}

// CHECK: class-template-instantiation-errors.h:18:7: error: no member named 'doesNotExist' in 'IntWrapper'
// CHECK: class-template-instantiation-errors.h:16:8: note: in instantiation of member function 'CannotBeInstantianted<IntWrapper>::willFailInstantiating' requested here
TemplatesTestSuite.test("clang-errors-reported-on-instantiation") {
    var _ = CannotBeInstantianted<IntWrapper>()
}

runAllTests()