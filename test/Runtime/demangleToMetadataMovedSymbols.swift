// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-stdlib %s -module-name main -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
// REQUIRES: OS=macosx

import Swift
import StdlibUnittest

let DemangleToMetadataMovedSymbolsTests = TestSuite("DemangleToMetadataMovedSymbols")

@available(OSX 10.9, *)
@_originallyDefinedIn(module: "foo", OSX 10.13)
public struct MovedS {
  struct Nested { }
}

@available(OSX 10.9, *)
@_originallyDefinedIn(module: "foo", OSX 10.13)
public enum MovedE { case e }

@available(OSX 10.9, *)
@_originallyDefinedIn(module: "bar", OSX 10.13)
public class MovedC {}

DemangleToMetadataMovedSymbolsTests.test("Moved Nominals") {
  // Simple Struct
  expectEqual(type(of: MovedS()), _typeByName("3foo6MovedSV")!)
  expectNil(_typeByName("4main6MovedSV"))

  // Simple Enum
  expectEqual(type(of: MovedE.e), _typeByName("3foo6MovedEO")!)
  expectNil(_typeByName("4main6MovedEO"))

  // Nested struct
  expectEqual(type(of: MovedS.Nested()), _typeByName("3foo6MovedSV6NestedV")!)
  expectNil(_typeByName("4main6MovedSV6NestedV"))

  // Simple Class
  expectEqual(type(of: MovedC()), _typeByName("3bar6MovedCC")!)
  expectNil(_typeByName("4main6MovedCC"))
}

runAllTests()
