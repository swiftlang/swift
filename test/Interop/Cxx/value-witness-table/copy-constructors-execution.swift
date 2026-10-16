// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default)
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

CXXCopyConstructorTestSuite.test("Copy constructor with default arguments") {
  // When in the presence of a C++ copy constructor with default args, we make the type non-copyable
  let originalObj = HasCopyConstructorWithDefaultArgs(5)
  expectEqual(originalObj.value, 5)

  // move originalObj
  let newObj = originalObj
  expectEqual(newObj.value, 5)
}

CXXCopyConstructorTestSuite.test("Copy constructor with one parameter that has a default argument") {
  // If the C++ copy constructor has exactly one param and it has a default argument, ignore the default argument and import the type as copyable to Swift

  let original = HasCopyConstructorWithOneParameterWithDefaultArg(1)
  let copy = original
  expectTrue(original.numCopies + copy.numCopies > 1)
}

enum BorrowedCaptureError: Error {
  case result(CInt)
}

extension HasUserProvidedCopyConstructor {
  borrowing func call(_ argument: CInt) -> CInt {
    withUnsafePointer(to: argument) { pointer in
      readWithArgument(self, pointer)
    }
  }

  borrowing func callNested(_ argument: CInt, shouldThrow: Bool) throws -> CInt {
    var result: CInt = 0
    return try withUnsafePointer(to: argument) { input in
      try withUnsafeMutablePointer(to: &result) { output in
        output.pointee = readWithArgument(self, input)
        if shouldThrow {
          throw BorrowedCaptureError.result(output.pointee)
        }
        return output.pointee
      }
    }
  }
}

CXXCopyConstructorTestSuite.test("Nonescaping closures borrow a C++ receiver") {
  let value = HasUserProvidedCopyConstructor(0)
  let initialCopies = getCopyConstructorCount().pointee
  expectEqual(42, value.call(42))
  expectEqual(initialCopies, getCopyConstructorCount().pointee)

  expectEqual(43, try! value.callNested(43, shouldThrow: false))
  expectEqual(initialCopies, getCopyConstructorCount().pointee)

  do {
    _ = try value.callNested(44, shouldThrow: true)
    expectUnreachable("expected a thrown result")
  } catch BorrowedCaptureError.result(let result) {
    expectEqual(44, result)
  } catch {
    expectUnreachable("unexpected error: \(error)")
  }
  expectEqual(initialCopies, getCopyConstructorCount().pointee)
  expectEqual(45, value.call(45))
  expectEqual(initialCopies, getCopyConstructorCount().pointee)
}

runAllTests()
