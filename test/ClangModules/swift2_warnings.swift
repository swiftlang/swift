// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -I %S/Inputs/custom-modules %s -verify

// REQUIRES: objc_interop


import Foundation

func testOldMethodNames(array: NSArray) {
  _ = array.indexOfObject(array)
}
