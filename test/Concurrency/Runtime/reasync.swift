// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/reasync.swiftmodule %S/Inputs/reasync.swift -enable-experimental-concurrency -target %target-swift-5.1-abi-triple
// RUN: %target-build-swift %s -I %t -o %t/main -module-name main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main

// REQUIRES: executable_test
// REQUIRES: concurrency

import StdlibUnittest
import reasync

var ReasyncTests = TestSuite("Reasync")

ReasyncTests.test("reasync") {
  var count = 0

  func rhs(_ b: Bool) -> Bool {
    count += 1
    return b
  }

  expectEqual(false, and(false, rhs(false)))
  expectEqual(count, 0)

  expectEqual(false, and(true, rhs(false)))
  expectEqual(count, 1)

  expectEqual(true, and(true, rhs(true)))
  expectEqual(count, 2)
}

ReasyncTests.test("reasyncNoThrows") {
  var count = 0

  func rhs(_ b: Bool) -> Bool {
    count += 1
    return b
  }

  expectEqual(false, andThrows(false, rhs(false)))
  expectEqual(count, 0)

  expectEqual(false, andThrows(true, rhs(false)))
  expectEqual(count, 1)

  expectEqual(true, andThrows(true, rhs(true)))
  expectEqual(count, 2)
}

enum CatError : Error {
  case napTime
}

ReasyncTests.test("reasyncThrows") {
  func throwError() throws -> Bool {
    throw CatError.napTime
  }

  expectEqual(false, try! andThrows(false, throwError()))

  do {
    _ = try andThrows(true, throwError())
    fatalError()
  } catch {}
}

runAllTests()
