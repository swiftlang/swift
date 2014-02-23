// RUN: %swift-ide-test -print-usrs -source-filename %s | FileCheck %s -strict-whitespace

// CHECK: [[@LINE+1]]:8 s:V14swift_ide_test1S{{$}}
struct S {
  // CHECK: [[@LINE+1]]:7 s:vV14swift_ide_test1S1xSi{{$}}
  var x : Int
}

// CHECK: [[@LINE+1]]:11 s:14swift_ide_test6MyGInt{{$}}
typealias MyGInt = Int

// CHECK: [[@LINE+1]]:7 s:C14swift_ide_test5MyCls{{$}}
class MyCls {
  // CHECK: [[@LINE+1]]:13 s:C14swift_ide_test5MyCls2TA{{$}}
  typealias TA = Int
  // CHECK: [[@LINE+1]]:7 s:vC14swift_ide_test5MyCls3wwwSi{{$}}
  var www : Int = 0
  // CHECK: [[@LINE+1]]:8 s:FC14swift_ide_test5MyCls3fooFS0_FT1xSi_T_{{$}}
  func foo(x : Int) {}
}

// CHECK: [[@LINE+1]]:10 s:P14swift_ide_test4Prot{{$}}
protocol Prot {
  // CHECK: [[@LINE+1]]:13 s:P14swift_ide_test4Prot5Blarg{{$}}
  typealias Blarg
  // CHECK: [[@LINE+1]]:8 s:FP14swift_ide_test4Prot8protMethUS0___FRQPS0_FT1xQS1_5Blarg_S2_{{$}}
  func protMeth(x: Blarg) -> Blarg
  // CHECK: [[@LINE+2]]:7 s:vP14swift_ide_test4Prot17protocolProperty1Si{{$}}
  // CHECK: [[@LINE+1]]:32 s:FP14swift_ide_test4Protg17protocolProperty1Si{{$}}
  var protocolProperty1: Int { get }
}
protocol Prot2 {}

class SubCls : MyCls, Prot {
  // CHECK: [[@LINE+1]]:13 s:C14swift_ide_test6SubCls5Blarg{{$}}
  typealias Blarg = Prot2
  // CHECK: [[@LINE+1]]:8 s:FC14swift_ide_test6SubCls8protMethFS0_FT1xPS_5Prot2__PS1__{{$}}
  func protMeth(x: Blarg) -> Blarg {}
  // CHECK: [[@LINE+1]]:7 s:vC14swift_ide_test6SubCls17protocolProperty1Si{{$}}
  var protocolProperty1 = 0
}

// CHECK: [[@LINE+1]]:6 s:F14swift_ide_test5genFnUS_4Prot__FT1pQ__Si{{$}}
func genFn<T : Prot where T.Blarg : Prot2>(p : T) -> Int {}

// CHECK: [[@LINE+1]]:6 s:F14swift_ide_test3barFT1xSi_TSiSf_{{$}}
func bar(x: Int) -> (Int, Float) {}

// CHECK: [[@LINE+1]]:7 s:C14swift_ide_test6GenCls{{$}}
class GenCls<T> {
  // CHECK: [[@LINE+1]]:3 s:FC14swift_ide_test6GenClscU__FMGS0_Q__FT_GS0_Q__{{$}}
  init() {}
  // CHECK: [[@LINE+1]]:3 s:FC14swift_ide_test6GenClsd{{$}}
  destructor() {}
  // CHECK: [[@LINE+1]]:14 s:FC14swift_ide_test6GenCls4cfooU__FMGS0_Q__FT_T_{{$}}
  class func cfoo() {}

  subscript (i : Int, j : Int) -> Int {
    // CHECK: [[@LINE+1]]:5 s:FC14swift_ide_test6GenClsg9subscriptFT1iSi1jSi_Si{{$}}
    get {
      return i + j
    }
    // CHECK: [[@LINE+1]]:5 s:FC14swift_ide_test6GenClss9subscriptFT1iSi1jSi_Si{{$}}
    set(v) {
      v + i - j
    }
  }
}

class C3 {
  // CHECK: [[@LINE+1]]:8 s:FC14swift_ide_test2C35multiFS0_FTSi9otherpartSi_T_{{$}}
  func multi(name: Int) otherpart(x: Int) {}
}

class C4 {
  // CHECK: [[@LINE+1]]:9 s:CC14swift_ide_test2C42In{{$}}
  class In {
    // CHECK: [[@LINE+1]]:16 s:FCC14swift_ide_test2C42In3gooFMS1_FT_T_{{$}}
    class func goo() {}
  }
}

class C5 {}
extension C5 {
  // CHECK: [[@LINE+1]]:8 s:FC14swift_ide_test2C55extFnFS0_FT_T_{{$}}
  func extFn() {}
}

class Observers {
  func doit() {}

  // CHECK: [[@LINE+1]]:7 s:vC14swift_ide_test9Observers2p1Si{{$}}
  var p1 : Int {
    // CHECK: [[@LINE+1]]:5 s:FC14swift_ide_test9Observersw2p1Si{{$}}
    willSet(newValue) { doit() }
    // CHECK: [[@LINE+1]]:5 s:FC14swift_ide_test9ObserversW2p1Si{{$}}
    didSet { doit() }
  }
}

// CHECK: [[@LINE+1]]:6 s:O14swift_ide_test5Suits{{$}}
enum Suits {
  // CHECK: [[@LINE+1]]:8 s:FO14swift_ide_test5Suits5ClubsFMS0_S0_{{$}}
  case Clubs
  // CHECK: [[@LINE+1]]:8 s:FO14swift_ide_test5Suits8DiamondsFMS0_S0_{{$}}
  case Diamonds

  // CHECK: [[@LINE+1]]:8 s:FO14swift_ide_test5Suits5enfooFS0_FT_T_{{$}}
  func enfoo() {}
}
