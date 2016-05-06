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
  _ = hive.makingHoney // expected-error{{'makingHoney' has been renamed to 'isMakingHoney'}}{{12-23=isMakingHoney}}
}

func testOldInitializerNames(array: NSArray) {
}

func testOldEnumCaseNames(i: Int) -> XMLNodeKind {
  switch i {
  case 0:
    return .InvalidKind // expected-error{{'InvalidKind' has been renamed to 'invalidKind'}}{{13-24=invalidKind}}

  case 1:
    return XMLNodeKind.InvalidKind // expected-error{{InvalidKind' has been renamed to 'invalidKind'}}{{24-35=invalidKind}}

  default:
    return .invalidKind
  }
}

func testOldOptionCaseNames(i: Int) -> RuncingOptions {
  switch i {
  case 0:
    return .EnableQuince // expected-error{{'EnableQuince' has been renamed to 'enableQuince'}}{{13-25=enableQuince}}

  case 1:
    return RuncingOptions.EnableMince // expected-error{{'EnableMince' has been renamed to 'enableMince'}}{{27-38=enableMince}}

  default:
    return .enableQuince
  }
}
