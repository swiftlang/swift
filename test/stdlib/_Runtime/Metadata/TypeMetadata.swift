// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: reflection_runtime
// UNSUPPORTED: freestanding

import StdlibUnittest
import _Runtime

let suite = TestSuite("TypeMetadata")

struct Struct<T> {
  let xyz: T
}

enum Enum<T> {
  case generic(T)
}

class Class<T> {
  let xyz: T
  
  init(xyz: T) {
    self.xyz = xyz
  }
}

#if canImport(Foundation)
import Foundation

class WeirdClass<T>: JSONDecoder {}
#endif

if #available(SwiftStdlib 5.9, *) {
  suite.test("Struct") {
    let meta = Metadata(Struct<Int>.self).type

    expectEqual(meta.descriptor.name, "Struct")

    let genericArg = meta.genericArguments.load(as: Any.Type.self)

    expectTrue(genericArg == Int.self)

    let fieldType = meta.descriptor.fields[0].typeRef
    expectTrue(meta.resolve(fieldType) == Int.self)
  }

  suite.test("Enum") {
    let meta = Metadata(Enum<String>.self).type

    expectEqual(meta.descriptor.name, "Enum")

    let genericArg = meta.genericArguments.load(as: Any.Type.self)

    expectTrue(genericArg == String.self)

    let caseType = meta.descriptor.fields[0].typeRef
    expectTrue(meta.resolve(caseType) == String.self)
  }

  suite.test("Class") {
    // Non resilient superclass

    let meta = Metadata(Class<Double>.self).type

    expectEqual(meta.descriptor.name, "Class")

    let genericArg = meta.genericArguments.load(as: Any.Type.self)

    expectTrue(genericArg == Double.self)

    let fieldTy = meta.descriptor.fields[0].typeRef
    expectTrue(meta.resolve(fieldTy) == Double.self)

    // Resilient superclass
#if canImport(Foundation)
    let weirdMeta = Metadata(WeirdClass<Float>.self).type

    expectEqual(weirdMeta.descriptor.name, "WeirdClass")

    let weirdGenericArg = weirdMeta.genericArguments.load(as: Any.Type.self)

    expectTrue(weirdGenericArg == Float.self)
#endif
  }
}

runAllTests()
