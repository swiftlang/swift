//===--- SwiftNativeNSBase.swift - Test __SwiftNativeNS*Base classes -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %empty-directory(%t)
// 
// RUN: %target-clang %S/Inputs/SwiftNativeNSBase/SwiftNativeNSBase.m -c -o %t/SwiftNativeNSBase.o -g
// RUN: %target-clang -fobjc-arc %S/Inputs/SlurpFastEnumeration/SlurpFastEnumeration.m -c -o %t/SlurpFastEnumeration.o
// RUN: echo '#sourceLocation(file: "%s", line: 1)' > "%t/main.swift" && cat "%s" >> "%t/main.swift" && chmod -w "%t/main.swift"
// RUN: %target-build-swift -Xfrontend -disable-access-control  %t/main.swift %S/Inputs/DictionaryKeyValueTypes.swift %S/Inputs/DictionaryKeyValueTypesObjC.swift -I %S/Inputs/SwiftNativeNSBase/ -I %S/Inputs/SlurpFastEnumeration/ -Xlinker %t/SlurpFastEnumeration.o -Xlinker %t/SwiftNativeNSBase.o -o %t/SwiftNativeNSBase -swift-version 4.2
// RUN: %target-codesign %t/SwiftNativeNSBase
// RUN: %target-run %t/SwiftNativeNSBase
// REQUIRES: executable_test

// REQUIRES: objc_interop

// The oldest ABI-stable stdlibs don't have a __SwiftNativeNSMutableArrayBase
// class, so they can't run the UnwantedCdtors test.
// FIXME: This should be based on a runtime library version check.
// UNSUPPORTED: use_os_stdlib

import Foundation
import StdlibUnittest

@_silgen_name("TestSwiftNativeNSBase_UnwantedCdtors") 
func TestSwiftNativeNSBase_UnwantedCdtors() -> Bool
@_silgen_name("TestSwiftNativeNSBase_RetainCount") 
func TestSwiftNativeNSBase_RetainCount(_: UnsafeMutableRawPointer) -> Bool

func classChain(of cls: AnyClass) -> [String] {
  var chain: [String] = []
  var cls: AnyClass? = cls
  while cls != nil {
    chain.append(NSStringFromClass(cls!))
    cls = class_getSuperclass(cls)
  }
  return chain
}

var SwiftNativeNSBaseTestSuite = TestSuite("SwiftNativeNSBase")

SwiftNativeNSBaseTestSuite.test("UnwantedCdtors") {
  expectTrue(TestSwiftNativeNSBase_UnwantedCdtors())
}

SwiftNativeNSBaseTestSuite.test("__SwiftNativeNSArrayBase.retainCount") {
  let bridged = getBridgedNSArrayOfRefTypeVerbatimBridged()
  assert(classChain(of: type(of: bridged)).contains("__SwiftNativeNSArrayBase"))
  expectTrue(TestSwiftNativeNSBase_RetainCount(
      Unmanaged.passUnretained(bridged).toOpaque()))
  _fixLifetime(bridged)
}

SwiftNativeNSBaseTestSuite.test("__SwiftNativeNSDictionaryBase.retainCount") {
  let bridged = getBridgedNSDictionaryOfRefTypesBridgedVerbatim()
  assert(classChain(of: type(of: bridged))
    .contains("__SwiftNativeNSDictionaryBase"))
  expectTrue(TestSwiftNativeNSBase_RetainCount(
      Unmanaged.passUnretained(bridged).toOpaque()))
  _fixLifetime(bridged)
}

SwiftNativeNSBaseTestSuite.test("__SwiftNativeNSSetBase.retainCount") {
  let bridged = Set([10, 20, 30].map{ TestObjCKeyTy($0) })._bridgeToObjectiveC()
  assert(classChain(of: type(of: bridged)).contains("__SwiftNativeNSSetBase"))
  expectTrue(TestSwiftNativeNSBase_RetainCount(
      Unmanaged.passUnretained(bridged).toOpaque()))
  _fixLifetime(bridged)
}

SwiftNativeNSBaseTestSuite.setUp {
  resetLeaksOfDictionaryKeysValues()
  resetLeaksOfObjCDictionaryKeysValues()
}

SwiftNativeNSBaseTestSuite.tearDown {
  expectNoLeaksOfDictionaryKeysValues()
  expectNoLeaksOfObjCDictionaryKeysValues()
}

runAllTests()
