// RUN: %empty-directory(%t)

// REQUIRES: executable_test

// 1. Build ../stored-properties.swift to a dylib and emit its interface in %t

// RUN: %target-build-swift-dylib(%t/%target-library-name(StoredProperties)) -emit-module-interface-path %t/StoredProperties.swiftinterface %S/stored-properties.swift -module-name StoredProperties -swift-version 5

// 2. Build this file and link with StoredProperties

// RUN: %target-build-swift %s -I %t -L %t -lStoredProperties -o %t/stored-properties-client %target-rpath(%t)

// 3. Codesign and run this, and ensure it exits successfully.

// RUN: %target-codesign %t/stored-properties-client %t/%target-library-name(StoredProperties)
// RUN: %target-run %t/stored-properties-client %t/%target-library-name(StoredProperties)

// 4. Repeat these steps, with library evolution enabled.

// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(StoredProperties)) -emit-module-interface-path %t/StoredProperties.swiftinterface %S/stored-properties.swift -module-name StoredProperties -swift-version 5 -enable-library-evolution

// RUN: %target-build-swift %s -I %t -L %t -lStoredProperties -o %t/stored-properties-client %target-rpath(%t)
// RUN: %target-codesign %t/stored-properties-client %t/%target-library-name(StoredProperties)
// RUN: %target-run %t/stored-properties-client %t/%target-library-name(StoredProperties)

import StdlibUnittest
import StoredProperties

/// This test makes sure clients of a parseable interface see correct type
/// layout and use the right access patterns in the presence of a
/// .swiftinterface file, in both resilient and non-resilient cases.

/// Test that we call the correct accessors in a resilient module, and that
/// we use trivial storage accesses in a non-resilient module.

var StoredPropertyTests = TestSuite("StoredProperty")

StoredPropertyTests.test("Getting/NonFixedLayout") {
  var value = HasStoredProperties()
  expectEqual(3, value.computedGetter)
  expectEqual(3, value.computedGetSet)
  expectEqual(0, value.simpleStoredImmutable)
  expectEqual(0, value.simpleStoredMutable)
  expectEqual(false, value.storedWithObservers)
  expectEqual(0, value.storedPrivateSet)
}

StoredPropertyTests.test("Setting/NonFixedLayout") {
  var value = HasStoredProperties()
  value.storedWithObservers = true
  value.computedGetSet = 4
  value.simpleStoredMutable = 4

  expectEqual(3, value.computedGetter)
  expectEqual(3, value.computedGetSet)
  expectEqual(0, value.simpleStoredImmutable)
  expectEqual(4, value.simpleStoredMutable)
  expectEqual(true, value.storedWithObservers)
  expectEqual(0, value.storedPrivateSet)
}

StoredPropertyTests.test("Getting/FixedLayout") {
  var value = HasStoredPropertiesFixedLayout()
  expectEqual(0, value.simpleStoredMutable.a)
  expectEqual(false, value.simpleStoredMutable.b)
  expectEqual(0, value.simpleStoredMutable.c)

  expectEqual(0, value.storedWithObservers.a)
  expectEqual(false, value.storedWithObservers.b)
  expectEqual(0, value.storedWithObservers.c)
}

StoredPropertyTests.test("Setting/FixedLayout") {
  var value = HasStoredPropertiesFixedLayout()

  value.simpleStoredMutable = BagOfVariables()
  expectEqual(0, value.simpleStoredMutable.a)
  expectEqual(false, value.simpleStoredMutable.b)
  expectEqual(0, value.simpleStoredMutable.c)

  value.storedWithObservers = BagOfVariables()
  expectEqual(0, value.storedWithObservers.a)
  expectEqual(false, value.storedWithObservers.b)
  expectEqual(0, value.storedWithObservers.c)
}

runAllTests()
