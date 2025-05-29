// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main  -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Distributed

struct SomeValue: Sendable, Codable {}

distributed actor Worker {
  distributed func empty() { }
  distributed func one(s: String) -> Int { 1337 }
  distributed func two(s: String, i: Int) -> Int { 1337 }
  distributed func three(s: String, i: Int, sv: SomeValue) -> Int { 1337 }
  distributed func hello(name: String) -> String { name }
}

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== Execute ----------------------------------------------------------------

@main struct Main {
  static let empty = "$s14SomeModuleName6WorkerC5emptyyyF"
  static let one = "$s14SomeModuleName6WorkerC3one1sSiSS_tF"
  static let two = "$s14SomeModuleName6WorkerC3two1s1iSiSS_SitF"

  static func test_count() {
    print("~~ \(#function)")
    // CHECK: _getParameterCount: empty() = 0
    print("_getParameterCount: empty() = \(_getParameterCount(mangledMethodName: empty))")
    // CHECK: _getParameterCount: one(s:) = 1
    print("_getParameterCount: one(s:) = \(_getParameterCount(mangledMethodName: one))")
    // CHECK: _getParameterCount: two(s:i:) = 2
    print("_getParameterCount: two(s:i:) = \(_getParameterCount(mangledMethodName: two))")
  }

  static func test_returnType() {
    print("~~ \(#function)")
    // CHECK: _getReturnTypeInfo: empty() = ()
    print("_getReturnTypeInfo: empty() = \(String(reflecting: _getReturnTypeInfo(mangledMethodName: empty, genericEnv: nil, genericArguments: nil)!))")

    // CHECK: _getReturnTypeInfo: one(s:) = Swift.Int
    print("_getReturnTypeInfo: one(s:) = \(String(reflecting: _getReturnTypeInfo(mangledMethodName: one, genericEnv: nil, genericArguments: nil)!))")
  }

  static func test_paramTypes() {
    print("~~ \(#function)")
    // CHECK: _withParameterTypeInfo: empty() = []
     _withParameterTypeInfo(mangledMethodName: empty) { params in
      print("_withParameterTypeInfo: empty() = \(params)")
    }

    // CHECK: _withParameterTypeInfo: one(s:) = [Swift.String]
    _withParameterTypeInfo(mangledMethodName: one) { params in
      print("_withParameterTypeInfo: one(s:) = \(params)")
    }

    // CHECK: _withParameterTypeInfo: two(s:i:) = [Swift.String, Swift.Int]
    _withParameterTypeInfo(mangledMethodName: two) { params in
      print("_withParameterTypeInfo: two(s:i:) = \(params)")
    }
  }

  static func main() {
    test_count()
    test_returnType()
    test_paramTypes()

    // CHECK: done
    print("done")
  }
}

func _withParameterTypeInfo(
    mangledMethodName name: String,
    body: ([Any.Type]) -> ()
) {
  let nameUTF8 = Array(name.utf8)

  return nameUTF8.withUnsafeBufferPointer { nameUTF8  in
    // 1) demangle to get the expected parameter count of the func
    let paramCount = __getParameterCount(nameUTF8.baseAddress!, UInt(nameUTF8.endIndex))

    guard paramCount > 0 else {
      body([])
      return
    }

    // prepare buffer for the parameter types to be decoded into:
    let infoBuffer = UnsafeMutableRawBufferPointer
        .allocate(byteCount: MemoryLayout<Any.Type>.size * Int(paramCount),
                  alignment: MemoryLayout<Any.Type>.alignment) // TODO: is this right always?
    defer {
      infoBuffer.deallocate()
    }

    // 2) demangle and write all parameter types into the prepared buffer
    let decodedNum = __getParameterTypeInfo(
        nameUTF8.baseAddress!, UInt(nameUTF8.endIndex),
        /*genericEnvironment=*/nil,
        /*genericArguments=*/nil,
        infoBuffer.baseAddress!._rawValue, Int(paramCount))

    // if we failed demangling the types, return an empty array
    guard decodedNum >= 0 else {
      body([])
      return
    }

    // copy the types from the buffer into a Swift Array
    var types: [Any.Type] = []
    types.reserveCapacity(Int(decodedNum))
      for i in infoBuffer.bindMemory(to: Any.Type.self) {
        types.append(i)
      }

    body(types)
    return
  }
}
