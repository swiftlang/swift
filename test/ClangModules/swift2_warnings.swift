// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -I %S/Inputs/custom-modules -enable-strip-ns-prefix %s -verify

// REQUIRES: objc_interop


import Foundation

func testOldTypeNames() {
  var ps: NSPostingStyle? // expected-error{{'NSPostingStyle' has been renamed to 'PostingStyle'}}{{11-25=PostingStyle}}


  _ = NSPostingStyle(rawValue: 1) // expected-error{{'NSPostingStyle' has been renamed to 'PostingStyle'}}{{7-21=PostingStyle}}
}

func testOldMethodNames(array: NSArray) {
  _ = array.indexOfObject(array) // expected-error{{'indexOfObject' has been renamed to 'index(of:)'}}{{13-26=index}}{{27-27=of: }}
}

func testOldPropertyNames(hive: Hive) {
  _ = hive.makingHoney // FIXME: Fix-It.
}

func testOldInitializerNames(array: NSArray) {
}

func testOldEnumCaseNames(i: Int) -> XMLNodeKind {
  switch i {
  case 0:
    return .InvalidKind // FIXME: Fix-It.

  case 1:
    return XMLNodeKind.InvalidKind // FIXME: Fix-It.

  default:
    return .invalidKind
  }
}
