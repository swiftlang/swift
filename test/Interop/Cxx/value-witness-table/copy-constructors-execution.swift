// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import CopyConstructors
import StdlibUnittest

var CXXCopyConstructorTestSuite = TestSuite("CXX Copy Constructor")

// It doesn't really matter how many copies were made (as long as it's at least
// one). What we really want to be checking is that the correct copy constructor
// was eventually called.

CXXCopyConstructorTestSuite.test("Basic object with custom copy constructor") {
  // Make sure we don't inline this function. We should copy "userCC" into the
  // two tuple elements (in the return address). If we inline this function, it
  // would allow Swift to put "userCC" directly into "expectTrue" which would
  // eliminate the copy.
  @inline(never)
  func createHasUserProvidedCopyConstructor() -> (HasUserProvidedCopyConstructor, HasUserProvidedCopyConstructor) {
    let userCC = HasUserProvidedCopyConstructor(0)
    return (userCC, userCC)
  }
  
  let result = createHasUserProvidedCopyConstructor()
  expectTrue(result.0.numCopies + result.1.numCopies > 0)
}

CXXCopyConstructorTestSuite.test("Implicit copy constructor, member with user-defined copy constructor") {
  @inline(never)
  func createTypeWithNonTrivialImplicitCopyConstructor() -> (HasNonTrivialImplicitCopyConstructor, HasNonTrivialImplicitCopyConstructor) {
    let implicit = HasNonTrivialImplicitCopyConstructor()
    return (implicit, implicit)
  }

  let result = createTypeWithNonTrivialImplicitCopyConstructor()
  expectTrue(result.0.box.numCopies + result.1.box.numCopies > 0)
}

CXXCopyConstructorTestSuite.test("Default copy constructor, member with user-defined copy constructor") {
  @inline(never)
  func createTypeWithNonTrivialDefaultCopyConstructor() -> (HasNonTrivialDefaultCopyConstructor, HasNonTrivialDefaultCopyConstructor) {
    let def = HasNonTrivialDefaultCopyConstructor()
    return (def, def)
  }
  
  let result = createTypeWithNonTrivialDefaultCopyConstructor()
  expectTrue(result.0.box.numCopies + result.1.box.numCopies > 0)
}

runAllTests()
