// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -disable-objc-attr-requires-foundation-module %s
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -new-mangling-for-tests -print-usrs -source-filename %s | %FileCheck %s -strict-whitespace
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-indexed-symbols -source-filename %s | %FileCheck %s -check-prefix=CHECK-PSEUDO

import macros

// CHECK: [[@LINE+1]]:8 s:14swift_ide_test1SV{{$}}
struct S {
  // CHECK: [[@LINE+1]]:7 s:14swift_ide_test1SV1xSiv{{$}}
  var x : Int
}

// CHECK: [[@LINE+1]]:11 s:14swift_ide_test6MyGInt{{$}}
typealias MyGInt = Int

// CHECK: [[@LINE+1]]:7 s:14swift_ide_test5MyClsC{{$}}
class MyCls {
  // CHECK: [[@LINE+1]]:13 s:14swift_ide_test5MyClsC2TA{{$}}
  typealias TA = Int
  // CHECK: [[@LINE+1]]:7 s:14swift_ide_test5MyClsC3wwwSiv{{$}}
  var www : Int = 0
  // CHECK: [[@LINE+1]]:8 s:14swift_ide_test5MyClsC3fooySiF{{$}}
  func foo(_ x : Int) {}
  // CHECK: [[@LINE+1]]:3 s:14swift_ide_test5MyClsC9subscriptSfSici{{$}}
  subscript(i: Int) -> Float {
    // CHECK: [[@LINE+1]]:5 s:14swift_ide_test5MyClsC9subscriptSfSicfg{{$}}
    get { return 0.0 }
    // CHECK: [[@LINE+1]]:5 s:14swift_ide_test5MyClsC9subscriptSfSicfs{{$}}
    set {}
  }
}

// CHECK: [[@LINE+1]]:7 s:14swift_ide_test12GenericClassC{{$}}
class GenericClass {

  // CHECK: [[@LINE+1]]:13 s:14swift_ide_test12GenericClassC2TA{{$}}
  typealias TA = Int

  // CHECK: [[@LINE+1]]:7 s:14swift_ide_test12GenericClassC11instanceVarSiv{{$}}
  var instanceVar: Int = 0

  // CHECK: [[@LINE+1]]:8 s:14swift_ide_test12GenericClassC12instanceFuncyyF{{$}}
  func instanceFunc() {
    // CHECK: [[@LINE+2]]:18 s:14swift_ide_test12GenericClassC9classFuncyACFZ{{$}}
    // CHECK: [[@LINE+1]]:28 s:14swift_ide_test12GenericClassC12instanceFuncyyF4selfL_ACv{{$}}
    GenericClass.classFunc(self)
  }

  // CHECK: [[@LINE+2]]:3 s:14swift_ide_test12GenericClassC9subscriptSfSici{{$}}
  // CHECK: [[@LINE+1]]:13 s:14swift_ide_test12GenericClassC1iL_Siv{{$}}
  subscript(i: Int) -> Float {
    // CHECK: [[@LINE+1]]:5 s:14swift_ide_test12GenericClassC9subscriptSfSicfg{{$}}
    get { return 0.0 }
    // CHECK: [[@LINE+1]]:5 s:14swift_ide_test12GenericClassC9subscriptSfSicfs{{$}}
    set {}
  }

  // CHECK: [[@LINE+1]]:3 s:14swift_ide_test12GenericClassCfd{{$}}
  deinit {
    // CHECK: [[@LINE+2]]:18 s:14swift_ide_test12GenericClassC9classFuncyACFZ{{$}}
    // CHECK: [[@LINE+1]]:28 ERROR:no-usr{{$}}
    GenericClass.classFunc(self)
  }

  // CHECK: [[@LINE+2]]:14 s:14swift_ide_test12GenericClassC9classFuncyACFZ{{$}}
  // CHECK: [[@LINE+1]]:26 s:14swift_ide_test12GenericClassC9classFuncyACFZ1aL_ACv{{$}}
  class func classFunc(_ a: GenericClass) {}
}

// CHECK: [[@LINE+1]]:10 s:14swift_ide_test4ProtP{{$}}
protocol Prot {
  // CHECK: [[@LINE+1]]:18 s:14swift_ide_test4ProtP5Blarg{{$}}
  associatedtype Blarg
  // CHECK: [[@LINE+1]]:8 s:14swift_ide_test4ProtP8protMeth5BlargQzAFF{{$}}
  func protMeth(_ x: Blarg) -> Blarg
  // CHECK: [[@LINE+2]]:7 s:14swift_ide_test4ProtP17protocolProperty1Siv{{$}}
  // CHECK: [[@LINE+1]]:32 s:14swift_ide_test4ProtP17protocolProperty1Sifg{{$}}
  var protocolProperty1: Int { get }
}
protocol Prot2 {}

class SubCls : MyCls, Prot {
  // CHECK: [[@LINE+1]]:13 s:14swift_ide_test6SubClsC5Blarg{{$}}
  typealias Blarg = Prot2
  // CHECK: [[@LINE+1]]:8 s:14swift_ide_test6SubClsC8protMethAA5Prot2_pAaE_pF{{$}}
  func protMeth(_ x: Blarg) -> Blarg {}
  // CHECK: [[@LINE+1]]:7 s:14swift_ide_test6SubClsC17protocolProperty1Siv{{$}}
  var protocolProperty1 = 0
}

// CHECK: [[@LINE+1]]:6 s:14swift_ide_test5genFnSixAA4ProtRzAA5Prot25BlargRpzlF{{$}}
func genFn<T : Prot>(_ p : T) -> Int where T.Blarg : Prot2 {}

// CHECK: [[@LINE+1]]:6 s:14swift_ide_test3barSi_SftSiF{{$}}
func bar(_ x: Int) -> (Int, Float) {}

// CHECK: [[@LINE+1]]:7 s:14swift_ide_test6GenClsC{{$}}
class GenCls<T> {
  // CHECK: [[@LINE+1]]:3 s:14swift_ide_test6GenClsCACyxGycfc{{$}}
  init() {}
  // CHECK: [[@LINE+1]]:3 s:14swift_ide_test6GenClsCfd{{$}}
  deinit {}
  // CHECK: [[@LINE+1]]:14 s:14swift_ide_test6GenClsC4cfooyyFZ{{$}}
  class func cfoo() {}

  // CHECK: [[@LINE+1]]:3 s:14swift_ide_test6GenClsC9subscriptS2i_Sitci{{$}}
  subscript (i : Int, j : Int) -> Int {
    // CHECK: [[@LINE+1]]:5 s:14swift_ide_test6GenClsC9subscriptS2i_Sitcfg{{$}}
    get {
      return i + j
    }
    // CHECK: [[@LINE+1]]:5 s:14swift_ide_test6GenClsC9subscriptS2i_Sitcfs{{$}}
    set(v) {
      _ = v + i - j
    }
  }
}

class C4 {
  // CHECK: [[@LINE+1]]:9 s:14swift_ide_test2C4C2InC{{$}}
  class In {
    // CHECK: [[@LINE+1]]:16 s:14swift_ide_test2C4C2InC3gooyyFZ{{$}}
    class func goo() {}
  }
}

class C5 {}
extension C5 {
  // CHECK: [[@LINE+1]]:8 s:14swift_ide_test2C5C5extFnyyF{{$}}
  func extFn() {}
}

class Observers {
  func doit() {}

  // CHECK: [[@LINE+1]]:7 s:14swift_ide_test9ObserversC2p1Siv{{$}}
  var p1 : Int = 0 {
    // CHECK: [[@LINE+1]]:5 s:14swift_ide_test9ObserversC2p1Sifw{{$}}
    willSet(newValue) { doit() }
    // CHECK: [[@LINE+1]]:5 s:14swift_ide_test9ObserversC2p1SifW{{$}}
    didSet { doit() }
  }

  // CHECK: [[@LINE+1]]:7 s:14swift_ide_test9ObserversC2p2Siv{{$}}
  var p2 = 42 {
    // CHECK: [[@LINE+1]]:5 s:14swift_ide_test9ObserversC2p2Sifw{{$}}
    willSet(newValue) { doit() }
    // CHECK: [[@LINE+1]]:5 s:14swift_ide_test9ObserversC2p2SifW{{$}}
    didSet { doit() }
  }
}

// CHECK: [[@LINE+2]]:7 c:@M@swift_ide_testobjc(cs)ObjCClass1{{$}}
@objc
class ObjCClass1 {
  // CHECK: [[@LINE+1]]:7 c:@M@swift_ide_testobjc(cs)ObjCClass1(py)instanceVar{{$}}
  var instanceVar: Int = 1
  // CHECK: [[@LINE+1]]:14 c:@M@swift_ide_testobjc(cs)ObjCClass1(cpy)typeVar{{$}}
  static var typeVar: Int = 1

  // CHECK: [[@LINE+1]]:7 c:@M@swift_ide_testobjc(cs)ObjCClass1(py)computed{{$}}
  var computed: Int {
    // CHECK: [[@LINE+1]]:5 c:@M@swift_ide_testobjc(cs)ObjCClass1(im)computed{{$}}
    get { return 1}
    // CHECK: [[@LINE+1]]:5 c:@M@swift_ide_testobjc(cs)ObjCClass1(im)setComputed:{{$}}
    set {}
  }

  // CHECK: [[@LINE+1]]:13 c:@M@swift_ide_testobjc(cs)ObjCClass1(cpy)typeComputed{{$}}
  class var typeComputed: Int {
    // CHECK: [[@LINE+1]]:5 c:@M@swift_ide_testobjc(cs)ObjCClass1(cm)typeComputed{{$}}
    get { return 1 }
    // CHECK: [[@LINE+1]]:5 c:@M@swift_ide_testobjc(cs)ObjCClass1(cm)setTypeComputed:{{$}}
    set {}
  }

  // CHECK: [[@LINE+1]]:3 c:@M@swift_ide_testobjc(cs)ObjCClass1(im)initWithX:{{$}}
  init(x: Int) {}
  // CHECK: [[@LINE+1]]:3 c:@M@swift_ide_testobjc(cs)ObjCClass1(im)init{{$}}
  init() {}

  // CHECK: [[@LINE+1]]:8 c:@M@swift_ide_testobjc(cs)ObjCClass1(im)instanceFunc1:{{$}}
  func instanceFunc1(_ a: Int) {

    // CHECK: [[@LINE+1]]:16 s:14swift_ide_test10ObjCClass1C13instanceFunc1ySiF9LocalEnumL_O
    @objc enum LocalEnum : Int {
      // CHECK: [[@LINE+1]]:12 s:14swift_ide_test10ObjCClass1C13instanceFunc1ySiF9LocalEnumL_O8someCaseA2FmF
      case someCase
    }
  }
  // CHECK: [[@LINE+1]]:14 c:@M@swift_ide_testobjc(cs)ObjCClass1(cm)staticFunc1:{{$}}
  class func staticFunc1(_ a: Int) {}

  // CHECK: [[@LINE+2]]:10 s:14swift_ide_test10ObjCClass1C9subscriptS2ici{{$}}
  // CHECK: [[@LINE+1]]:20 s:14swift_ide_test10ObjCClass1C1xL_Siv{{$}}
  public subscript(x: Int) -> Int {

    // CHECK: [[@LINE+2]]:5 c:@M@swift_ide_testobjc(cs)ObjCClass1(im)objectAtIndexedSubscript:{{$}}
    // CHECK-PSEUDO: [[@LINE+1]]:18 {{.*}} | c:@M@swift_ide_testobjc(cs)ObjCClass1(im)instanceVar | {{.*}}
    get { return instanceVar }

    // CHECK: [[@LINE+1]]:5 c:@M@swift_ide_testobjc(cs)ObjCClass1(im)setObject:atIndexedSubscript:{{$}}
    set {}
  }

  // CHECK: [[@LINE+2]]:10 s:14swift_ide_test10ObjCClass1C9subscriptSiACci{{$}}
  // CHECK: [[@LINE+1]]:20 s:14swift_ide_test10ObjCClass1C1xL_ACv{{$}}
  public subscript(x: ObjCClass1) -> Int {

    // CHECK: [[@LINE+1]]:5 c:@M@swift_ide_testobjc(cs)ObjCClass1(im)objectForKeyedSubscript:{{$}}
    get { return 1 }

    // CHECK: [[@LINE+1]]:5 c:@M@swift_ide_testobjc(cs)ObjCClass1(im)setObject:forKeyedSubscript:{{$}}
    set {}
  }

  // Nested types are not officially supported
  // CHECK: [[@LINE+1]]:15 s:14swift_ide_test10ObjCClass1C6NestedC{{$}}
  @objc class Nested {}
}

// CHECK: [[@LINE+1]]:23 c:@M@swift_ide_testobjc(pl)ObjCProto{{$}}
@objc public protocol ObjCProto {

  // CHECK: [[@LINE+1]]:8 c:@M@swift_ide_testobjc(pl)ObjCProto(im)protoMeth{{$}}
  func protoMeth()
}

// CHECK: [[@LINE+1]]:12 c:@M@swift_ide_test@E@ObjCEnum{{$}}
@objc enum ObjCEnum : Int {

  // CHECK: [[@LINE+1]]:8 c:@M@swift_ide_test@E@ObjCEnum@ObjCEnumAmazingCase{{$}}
  case amazingCase
}

extension ObjCClass1 {
  // CHECK: [[@LINE+1]]:15 c:@M@swift_ide_testobjc(cs)ObjCClass1(im)objcExtMethodWithX:{{$}}
  public func objcExtMethod(x: Int) {}
}

// CHECK: [[@LINE+1]]:6 s:14swift_ide_test5SuitsO{{$}}
enum Suits {
  // CHECK: [[@LINE+1]]:8 s:14swift_ide_test5SuitsO5ClubsA2CmF{{$}}
  case Clubs
  // CHECK: [[@LINE+1]]:8 s:14swift_ide_test5SuitsO8DiamondsA2CmF{{$}}
  case Diamonds

  // CHECK: [[@LINE+1]]:8 s:14swift_ide_test5SuitsO5enfooyyF{{$}}
  func enfoo() {}
}

func importedMacros() {
  // CHECK: [[@LINE+1]]:12 c:@macro@M_PI{{$}}
  let m1 = M_PI
  // CHECK: [[@LINE+1]]:12 c:@macro@MACRO_FROM_IMPL{{$}}
  let m2 = MACRO_FROM_IMPL
  // CHECK: [[@LINE+1]]:12 c:@macro@USES_MACRO_FROM_OTHER_MODULE_1{{$}}
  let m3 = USES_MACRO_FROM_OTHER_MODULE_1
  
  _ = m1; _ = m2; _ = m3
}

