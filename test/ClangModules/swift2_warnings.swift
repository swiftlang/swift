// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %S/../IDE/Inputs/custom-modules) -emit-sil -I %S/Inputs/custom-modules -enable-strip-ns-prefix %s -verify

// REQUIRES: objc_interop


import Foundation
import ImportAsMember.A

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
