// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import ClassTemplateEagerInstantiationProblems
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("eager-instantiation-of-members") {
  // This will fail with:
  //
  // error: type 'int' cannot be used prior to '::' because it has no members
  //   T::getIntDoesNotExist();
  //
  // whereas in C++ this compiles. This is caused by ClangImporter eagerly
  // instantiating typedeffed templates and also their members.
  // TODO(scentini): Fix this
  // let _brokenMagicWrapper = BrokenMagicWrapper()
}

TemplatesTestSuite.test("sfinae-example") {
  // This will fail since we are currently not instantiating function templates.
  // In C++ the first sfinaeGetInt should fail to instantiate, therefore get
  // ignored, and only the second sfinaeGetInt is used.
  // TODO(SR-12541): Fix this
  // let magicNumber = MagicNumber()
  // var brokenMagicWrapper = BrokenMagicWrapper()
  // expectEqual(42, brokenMagicWrapper.sfinaeGetInt(magicNumber, 0))
}

runAllTests()
