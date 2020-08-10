// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -I %S/Inputs -o %t/address_only -Xfrontend -enable-cxx-interop
// RUN: %target-codesign %t/address_only
// RUN: %target-run %t/address_only 2&>1

// REQUIRES: executable_test

import TypeClassification
import StdlibUnittest

var AddressOnlyTestSuite = TestSuite("Address Only Types")

AddressOnlyTestSuite.test("Test struct with copy constructor") {
  let obj = StructWithCopyConstructorAndValue(value: 42)
  expectEqual(obj.value, 42)
}

AddressOnlyTestSuite.test("Test struct with member with copy constructor") {
  let member = StructWithCopyConstructorAndValue(value: 42)
  let obj = StructWithSubobjectCopyConstructorAndValue(member: member)
  expectEqual(obj.member.value, 42)
}

AddressOnlyTestSuite.test(
  "Test struct with copy constructor and member with copy constructor"
) {
  let member = StructWithCopyConstructorAndValue(value: 42)
  let obj = StructWithCopyConstructorAndSubobjectCopyConstructorAndValue(
    member: member
  )
  expectEqual(obj.member.value, 42)
}

runAllTests()

