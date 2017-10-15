// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest

import Foundation

var tests = TestSuite("NSItemProvider")

if #available(OSX 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *) {

  tests.test("overlay") {
    let itemProvider = NSItemProvider()

    let string = "Hello"
    let url = URL(string: "https://www.apple.com")

    itemProvider.registerObject(ofClass: String.self, visibility: .all) {
      (completionBlock) in
        completionBlock(string, nil)
        return nil
    }

    itemProvider.registerObject(ofClass: URL.self, visibility: .all) {
      (completionBlock) in
        completionBlock(url, nil)
        return nil
    }

    expectTrue(itemProvider.canLoadObject(ofClass: String.self))
    expectTrue(itemProvider.canLoadObject(ofClass: URL.self))

//  let expectLoadString = expectation("loadString")
    _ = itemProvider.loadObject(ofClass: String.self) {
      (string, error) in
        expectNotNil(string)
        expectNil(error)
//      expectLoadString.fulfill()
    }

//  let expectLoadURL = expectation("loadURL")
    _ = itemProvider.loadObject(ofClass: String.self) {
      (url, error) in
        expectNotNil(url)
        expectNil(error)
//      expectLoadURL.fulfill()
    }

//  wait(for: [expectLoadString, expectLoadURL], 1.0)
  }
}

runAllTests()
