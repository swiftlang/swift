// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %S/../IDE/Inputs/custom-modules) -emit-sil -I %S/Inputs/custom-modules %s -verify -verify-ignore-unknown

// REQUIRES: objc_interop


import Foundation
import ImportAsMember.A
import AppKit

func testOldTypeNames() {
  var ps: NSPostingStyle? // expected-error{{'NSPostingStyle' has been renamed to 'NotificationQueue.PostingStyle'}}{{11-25=NotificationQueue.PostingStyle}}


  _ = NSPostingStyle(rawValue: 1) // expected-error{{'NSPostingStyle' has been renamed to 'NotificationQueue.PostingStyle'}}{{7-21=NotificationQueue.PostingStyle}}

  _ = NSOperation() // expected-error{{'NSOperation' has been renamed to 'Operation'}}{{7-18=Operation}}
}

func testOldMethodNames(array: NSArray) {
  _ = array.indexOfObject(array) // expected-error{{'indexOfObject' has been renamed to 'index(of:)'}}{{13-26=index}}{{27-27=of: }}
}

func testOldPropertyNames(hive: Hive) {
  _ = hive.makingHoney // expected-error{{'makingHoney' has been renamed to 'isMakingHoney'}}{{12-23=isMakingHoney}}
}

func testOldInitializerNames(array: NSArray) {
}

func testOldEnumCaseNames(i: Int) -> XMLNode.Kind {
  switch i {
  case 0:
    // FIXME: Bad diagnostic.
    return .InvalidKind // expected-error{{type 'XMLNode.Kind' has no member 'InvalidKind'}}

  case 1:
    // FIXME: Bad diagnostic.
    return XMLNode.Kind.InvalidKind // expected-error{{type 'XMLNode.Kind' has no member 'InvalidKind'}}

  default:
    return .invalid
  }
}

func testOldOptionCaseNames(i: Int) -> NSRuncingOptions {
  switch i {
  case 0:
    return .EnableQuince // expected-error{{'EnableQuince' has been renamed to 'enableQuince'}}{{13-25=enableQuince}}

  case 1:
    return NSRuncingOptions.EnableMince // expected-error{{'EnableMince' has been renamed to 'enableMince'}}{{29-40=enableMince}}

  default:
    return .enableQuince
  }
}

func testImportAsMember() {
  _ = IAMStruct1GlobalVar // expected-error{{'IAMStruct1GlobalVar' has been renamed to 'Struct1.globalVar'}}{{7-26=Struct1.globalVar}}
  _ = IAMStruct1CreateSimple(1.5)
  // expected-error@-1{{'IAMStruct1CreateSimple' has been replaced by 'Struct1.init(value:)'}}{{7-29=Struct1}}{{30-30=value: }}

  var iam1 = Struct1(value: 1.5)
  _ = IAMStruct1Invert(iam1)
  // expected-error@-1{{'IAMStruct1Invert' has been replaced by instance method 'Struct1.inverted()'}}{{7-23=iam1.inverted}}{{24-28=}}

  IAMStruct1InvertInPlace(&iam1)
  // expected-error@-1{{'IAMStruct1InvertInPlace' has been replaced by instance method 'Struct1.invert()'}}{{3-26=(&iam1).invert}}{{27-32=}}
  // FIXME: "&" part has to be removed for mutating methods.

  _ = IAMStruct1Rotate(&iam1, 3.14159)
  // expected-error@-1{{'IAMStruct1Rotate' has been replaced by instance method 'Struct1.translate(radians:)'}}{{7-23=(&iam1).translate}} {{24-31=}} {{31-31=radians: }}
  // FIXME: "&" part has to be removed for mutating methods.

  _ = IAMStruct1StaticMethod()
  // expected-error@-1{{'IAMStruct1StaticMethod()' has been replaced by 'Struct1.staticMethod()'}}{{7-29=Struct1.staticMethod}}

  _ = IAMStruct1GetRadius(&iam1)
  // expected-error@-1{{'IAMStruct1GetRadius' has been replaced by property 'Struct1.radius'}}{{7-26=(&iam1).radius}} {{26-33=}}
  // FIXME: &iam1 is wrong

  IAMStruct1SetRadius(iam1, 3.14159)
  // expected-error@-1{{'IAMStruct1SetRadius' has been replaced by property 'Struct1.radius'}}{{3-22=iam1.radius}} {{22-29= = }} {{36-37=}}
}

// rdar://problem/26236989
class X : NSDocument {
  func test(url: URL) {
  }
  func test2() {
    let url = URL(string: "ABC")
    self.url = url!
  }
  func getTheURL() -> URL {
    return url
  }
}

func makeProgress<T: NSProgressReporting>(thing: T) {} // expected-error {{'NSProgressReporting' has been renamed to 'ProgressReporting'}} {{22-41=ProgressReporting}} 

func useLowercasedEnumCase(x: NSRuncingMode) {
  switch x {
    case .Mince: return // expected-error {{'Mince' has been renamed to 'mince'}} {{11-16=mince}}
    case .Quince: return // expected-error {{'Quince' has been renamed to 'quince'}} {{11-17=quince}}
  }
}

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected note produced: 'NSProgressReporting' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: 'NSPostingStyle' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: 'NSPostingStyle' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: 'NSOperation' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: 'indexOfObject' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: 'makingHoney' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: did you mean 'NSXMLInvalidKind'?
// <unknown>:0: error: unexpected note produced: did you mean 'NSXMLInvalidKind'?
// <unknown>:0: error: unexpected note produced: 'EnableQuince' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: 'EnableMince' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: 'IAMStruct1GlobalVar' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: 'IAMStruct1CreateSimple' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: 'IAMStruct1Invert' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: 'IAMStruct1InvertInPlace' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: 'IAMStruct1Rotate' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: 'IAMStruct1StaticMethod()' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: 'IAMStruct1GetRadius' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: 'IAMStruct1SetRadius' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: 'Mince' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: 'Quince' was obsoleted in Swift 3
