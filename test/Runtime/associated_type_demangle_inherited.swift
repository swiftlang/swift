// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-stdlib %s -module-name main -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

import Swift
import StdlibUnittest

class Key<T>: RawRepresentable {
  typealias RawValue = T

  let rawValue: T

  required init(rawValue: T) {
    self.rawValue = rawValue
  }
}

extension Key: Hashable where T: Hashable {
    func hash(into hasher: inout Hasher) {
        hasher.combine(rawValue)
    }
}

extension Key: Equatable where T: Equatable {
    static func == (lhs: Key, rhs: Key) -> Bool {
        return lhs.rawValue == rhs.rawValue
    }
}

class SpecificKey: Key<String> { }

extension SpecificKey {
    static let name = SpecificKey(rawValue: "name")
}

let AssociatedTypeDemangleTests = TestSuite("AssociatedTypeDemangle")

AssociatedTypeDemangleTests.test("superclass substitutions") {
  var dictionary: [SpecificKey: String] = [:]
  dictionary[SpecificKey.name] = "Hello"

  expectEqual(["Hello"], dictionary.values.map { $0 })
}

runAllTests()
