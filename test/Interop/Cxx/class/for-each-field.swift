// RUN: %target-run-simple-swift(-cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -I %S/Inputs)

// REQUIRES: executable_test
// REQUIRES: reflection

@_spi(Reflection) import Swift
import SimpleStructs
import StdlibUnittest

func checkFieldsWithKeyPath<T>(
  of type: T.Type,
  options: _EachFieldOptions = [],
  fields: [String: PartialKeyPath<T>]
) {
  var count = 0

  _forEachFieldWithKeyPath(of: T.self, options: options) {
    charPtr, keyPath in
    count += 1

    let fieldName = String(cString: charPtr)
    if fieldName == "" {
      expectTrue(false, "Empty field name")
      return true
    }
    guard let checkKeyPath = fields[fieldName] else {
      expectTrue(false, "Unexpected field '\(fieldName)'")
      return true
    }

    expectTrue(checkKeyPath == keyPath)
    return true
  }

  expectEqual(fields.count, count)
}

var ForEachFieldTestSuite = TestSuite("ForEachField")

ForEachFieldTestSuite.test("HasPrivateFieldsOnly") {
  checkFieldsWithKeyPath(
    of: HasPrivateFieldsOnly.self,
    fields: [:]
  )
}

ForEachFieldTestSuite.test("HasPublicFieldsOnly") {
  checkFieldsWithKeyPath(
    of: HasPublicFieldsOnly.self,
    fields: [
      "publ1": \HasPublicFieldsOnly.publ1,
      "publ2": \HasPublicFieldsOnly.publ2
    ]
  )
}

ForEachFieldTestSuite.test("HasPrivatePublicProtectedFields") {
  checkFieldsWithKeyPath(
    of: HasPrivatePublicProtectedFields.self,
    fields: [
      "publ1": \HasPrivatePublicProtectedFields.publ1,
      "publ2": \HasPrivatePublicProtectedFields.publ2
    ]
  )
}

ForEachFieldTestSuite.test("Outer") {
  checkFieldsWithKeyPath(
    of: Outer.self,
    fields: [
      "publStruct": \Outer.publStruct
    ]
  )
}

runAllTests()
