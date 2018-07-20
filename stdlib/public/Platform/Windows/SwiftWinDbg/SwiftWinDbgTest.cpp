//===-- Windows/SwiftWinDbgTest.cpp - Tests SwiftWinDbg.cpp -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This contains tests for SwiftWinDbg.cpp
///
//===----------------------------------------------------------------------===//
#include "CppUnitTest.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

char *extractString(uint64_t StringObject, uint64_t StringOtherBits,
                    uint64_t &Count, uint64_t &PayloadAddress);

namespace SwiftWinDbgTest {

TEST_CLASS(StringUnitTest) {
public:
  // NOTE: Use this Swift function to get the internal bits of a `String` and
  //       generate new testcases.
  // // Print out the internal type and internal bits of a Swift String.
  // func printStringGuts(_ str: String) {
  //   var strCopy = str
  //   let _guts = UnsafeMutableRawPointer(&strCopy)
  //   let _object = _guts.load(as: UInt64.self)
  //   let _otherBits = _guts.load(fromByteOffset: 8, as: UInt64.self)
  //   let objectHex = String(_object, radix: 16).uppercased()
  //   let otherBitsHex = String(_otherBits, radix: 16).uppercased()
  // 
  //   func getStringType(_ bits: UInt64) -> String {
  //     let isValue = bits & (1 << 63) != 0
  //     let isCocoaOrSmall = bits & (1 << 62) != 0
  //     let isOpaque = bits & (1 << 61) != 0
  //     if (!isOpaque && !isCocoaOrSmall) {
  //       return "Native"
  //     } else if (isCocoaOrSmall && !isValue) {
  //       return "NSString"
  //     } else if (isCocoaOrSmall && isValue && !isOpaque) {
  //       return "NSString Inlined"
  //     } else {
  //       return "UTF-8 Inlined"
  //     }
  //   }
  //   let type = getStringType(_object)
  // 
  //   print("\"\(str)\" (\(type)) = {_object = 0x\(objectHex), " +
  //                                 "_otherBits = 0x\(otherBitsHex)}")
  // }

  TEST_METHOD(InlinedString){
    uint64_t StringObject, StringOtherBits, Count, PayloadAddress;
    char *StringBuffer;

    // printStringGuts("<= 15 chars")
    StringObject = 0xEB00000000737261;
    StringOtherBits = 0x6863203531203D3C;

    StringBuffer = extractString(StringObject, StringOtherBits,
                                 Count, PayloadAddress);
    Assert::AreEqual("<= 15 chars", StringBuffer);
    Assert::AreEqual((uint64_t)11, Count);

    // printStringGuts("Hello ...World?")
    StringObject = 0xEF3F646C726F572E;
    StringOtherBits = 0x2E2E206F6C6C6548;

    StringBuffer = extractString(StringObject, StringOtherBits,
                                 Count, PayloadAddress);
    Assert::AreEqual("Hello ...World?", StringBuffer);
    Assert::AreEqual((uint64_t)15, Count);
  }

  TEST_METHOD(NativeString) {
    uint64_t StringObject, StringOtherBits, Count, PayloadAddress;
    char *StringBuffer;

    // printStringGuts("This is a very long Native Swift String")
    StringObject = 0x80007F4DD4E2B040;
    StringOtherBits = 0x27;

    StringBuffer = extractString(StringObject, StringOtherBits,
                                 Count, PayloadAddress);
    Assert::AreEqual(nullptr, StringBuffer);
    Assert::AreEqual((uint64_t)39, Count);
    Assert::AreEqual((uint64_t)0x00007F4DD4E2B040, PayloadAddress);
  }
};

}