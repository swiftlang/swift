// RUN: mkdir -p %t
// RUN: %clang -fobjc-arc %S/Inputs/enum-new.m -c -o %t/enum-new.o
// RUN: %target-build-swift -import-objc-header %S/Inputs/enum-new.h -Xlinker %t/enum-new.o %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s

// REQUIRES: executable_test

import Foundation

func printError(err: TestError) {
  switch (err) {
    case .TENone:
    print("TestError: TENone")
    break

    case .TEOne:
    print("TestError: TEOne")
    break

    case .TETwo:
    print("TestError: TETwo")
    break
  }
}

func testError() {
  let terr = getErr()
  switch (terr) { case .TENone, .TEOne, .TETwo: break } // ok
  printError(terr)
    // CHECK: TestError: TEOne

  do {
    throw TestError.TETwo
  } catch let error as TestError {
    printError(error)
    // CHECK-NEXT: TestError: TETwo
  } catch {
    assert(false)
  }

  do {
    enum LocalError : ErrorType { case Err }
    throw LocalError.Err
  } catch let error as TestError {
    printError(error)
  } catch {
    print("other error found")
    // CHECK-NEXT: other error found
  }
}

testError()