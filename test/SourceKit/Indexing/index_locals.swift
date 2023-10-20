// RUN: %empty-directory(%t)
// RUN: %sourcekitd-test -req=index-to-store %s -index-store-path %t/idx -index-unit-output-path %t/index_locals.o -req-opts=should_index_locals=1 -- %s
// RUN: c-index-test core -print-record %t/idx | %FileCheck %s


func foo(a: Int, b: Double) {
    var locVar = 1
}

var globalVar = 2

class A : ExpressibleByIntegerLiteral {
    required init(integerLiteral value: IntegerLiteralType) {}
}

func bar() {
    var locVar1: A = 1
    let locVar2 = 2
    foo(a: globalVar, b: Double(locVar2))
    let closure = { (arg1: Int, arg2: Int) -> Int in
         let locVar3 = 3
         let expr: Int = locVar2 + globalVar + arg1
         return expr
    }
}

class X {
    func baz() {
        func inner(_ x: Int) -> Int { return x }
        var arr: [Int] = [1, 2, 3]
        if (globalVar > 2) {
            arr = arr.map { inner($0) }
        }
    }
}


// CHECK: function/Swift | foo(a:b:) | s:12index_locals3foo1a1bySi_SdtF | <no-cgname> | Def,Ref,Call,RelCall,RelCont - RelChild,RelCont
// CHECK-NEXT: param/Swift | a | s:12index_locals3foo1a1bySi_SdtFACL_Sivp | <no-cgname> | Def,RelChild -
// CHECK-NEXT: struct/Swift | Int | s:Si | <no-cgname> | Ref,RelCont -
// CHECK-NEXT: param/Swift | b | s:12index_locals3foo1a1bySi_SdtFADL_Sdvp | <no-cgname> | Def,RelChild -
// CHECK-NEXT: struct/Swift | Double | s:Sd | <no-cgname> | Ref,RelCont -
// CHECK-NEXT: function/acc-get(local)/Swift | getter:locVar | s:12index_locals3foo1a1bySi_SdtF6locVarL_Sivg | <no-cgname> | Def,Impl,RelChild,RelAcc -
// CHECK-NEXT: variable(local)/Swift | locVar | s:12index_locals3foo1a1bySi_SdtF6locVarL_Sivp | <no-cgname> | Def,RelChild - RelChild,RelAcc
// CHECK-NEXT: function/acc-set(local)/Swift | setter:locVar | s:12index_locals3foo1a1bySi_SdtF6locVarL_Sivs | <no-cgname> | Def,Impl,RelChild,RelAcc -
// CHECK-NEXT: function/acc-get/Swift | getter:globalVar | s:12index_locals9globalVarSivg | <no-cgname> | Def,Ref,Call,Impl,RelChild,RelCall,RelAcc,RelCont -
// CHECK-NEXT: variable/Swift | globalVar | s:12index_locals9globalVarSivp | <no-cgname> | Def,Ref,Read,RelCont - RelChild,RelAcc
// CHECK-NEXT: function/acc-set/Swift | setter:globalVar | s:12index_locals9globalVarSivs | <no-cgname> | Def,Impl,RelChild,RelAcc -
// CHECK-NEXT: class/Swift | A | s:12index_locals1AC | <no-cgname> | Def,Ref,RelCont - RelChild,RelBase
// CHECK-NEXT: protocol/Swift | ExpressibleByIntegerLiteral | s:s27ExpressibleByIntegerLiteralP | <no-cgname> | Ref,RelBase -
// CHECK-NEXT: constructor/Swift | init(integerLiteral:) | s:12index_locals1AC14integerLiteralACSi_tcfc | <no-cgname> | Def,Ref,Call,Impl,RelChild,RelOver,RelCall,RelCont - RelChild,RelCont
// CHECK-NEXT: constructor/Swift | init(integerLiteral:) | s:s27ExpressibleByIntegerLiteralP07integerD0x0cD4TypeQz_tcfc | <no-cgname> |  - RelOver
// CHECK-NEXT: param(local)/Swift | value | s:12index_locals1AC14integerLiteralACSi_tcfc5valueL_Sivp | <no-cgname> | Def,RelChild -
// CHECK-NEXT: type-alias/Swift | IntegerLiteralType | s:s18IntegerLiteralTypea | <no-cgname> | Ref,RelCont -
// CHECK-NEXT: function/Swift | bar() | s:12index_locals3baryyF | <no-cgname> | Def - RelChild,RelCall,RelCont
// CHECK-NEXT: function/acc-get(local)/Swift | getter:locVar1 | s:12index_locals3baryyF7locVar1L_AA1ACvg | <no-cgname> | Def,Impl,RelChild,RelAcc -
// CHECK-NEXT: variable(local)/Swift | locVar1 | s:12index_locals3baryyF7locVar1L_AA1ACvp | <no-cgname> | Def,RelChild - RelChild,RelAcc,RelCont
// CHECK-NEXT: function/acc-set(local)/Swift | setter:locVar1 | s:12index_locals3baryyF7locVar1L_AA1ACvs | <no-cgname> | Def,Impl,RelChild,RelAcc -
// CHECK-NEXT: function/acc-get(local)/Swift | getter:locVar2 | s:12index_locals3baryyF7locVar2L_Sivg | <no-cgname> | Def,Ref,Call,Impl,RelChild,RelCall,RelAcc,RelCont -
// CHECK-NEXT: variable(local)/Swift | locVar2 | s:12index_locals3baryyF7locVar2L_Sivp | <no-cgname> | Def,Ref,Read,RelChild,RelCont - RelChild,RelAcc
// CHECK-NEXT: function/acc-set(local)/Swift | setter:locVar2 | s:12index_locals3baryyF7locVar2L_Sivs | <no-cgname> | Def,Impl,RelChild,RelAcc -
// CHECK-NEXT: constructor/Swift | init(_:) | s:SdySdSicfc | <no-cgname> | Ref,Call,RelCall,RelCont -
// CHECK-NEXT: function/acc-get(local)/Swift | getter:closure | s:12index_locals3baryyF7closureL_yS2i_Sitcvg | <no-cgname> | Def,Impl,RelChild,RelAcc -
// CHECK-NEXT: variable(local)/Swift | closure | s:12index_locals3baryyF7closureL_yS2i_Sitcvp | <no-cgname> | Def,RelChild - RelChild,RelAcc,RelCont
// CHECK-NEXT: function/acc-set(local)/Swift | setter:closure | s:12index_locals3baryyF7closureL_yS2i_Sitcvs | <no-cgname> | Def,Impl,RelChild,RelAcc -
// CHECK-NEXT: param(local)/Swift | arg1 | s:12index_locals3baryyFS2i_SitcfU_4arg1L_Sivp | <no-cgname> | Def,Ref,Read,RelChild,RelCont -
// CHECK-NEXT: param(local)/Swift | arg2 | s:12index_locals3baryyFS2i_SitcfU_4arg2L_Sivp | <no-cgname> | Def,RelChild -
// CHECK-NEXT: function/acc-get(local)/Swift | getter:locVar3 | s:12index_locals3baryyFS2i_SitcfU_7locVar3L_Sivg | <no-cgname> | Def,Impl,RelChild,RelAcc -
// CHECK-NEXT: variable(local)/Swift | locVar3 | s:12index_locals3baryyFS2i_SitcfU_7locVar3L_Sivp | <no-cgname> | Def,RelChild - RelChild,RelAcc
// CHECK-NEXT: function/acc-set(local)/Swift | setter:locVar3 | s:12index_locals3baryyFS2i_SitcfU_7locVar3L_Sivs | <no-cgname> | Def,Impl,RelChild,RelAcc -
// CHECK-NEXT: function/acc-get(local)/Swift | getter:expr | s:12index_locals3baryyFS2i_SitcfU_4exprL_Sivg | <no-cgname> | Def,Ref,Call,Impl,RelChild,RelCall,RelAcc,RelCont -
// CHECK-NEXT: variable(local)/Swift | expr | s:12index_locals3baryyFS2i_SitcfU_4exprL_Sivp | <no-cgname> | Def,Ref,Read,RelChild,RelCont - RelChild,RelAcc,RelCont
// CHECK-NEXT: function/acc-set(local)/Swift | setter:expr | s:12index_locals3baryyFS2i_SitcfU_4exprL_Sivs | <no-cgname> | Def,Impl,RelChild,RelAcc -
// CHECK-NEXT: static-method/infix-operator/Swift | +(_:_:) | s:Si1poiyS2i_SitFZ | <no-cgname> | Ref,Call,RelCall,RelCont -
// CHECK-NEXT: constructor/Swift | init() | s:12index_locals1XCACycfc | <no-cgname> | Def,Impl,RelChild -
// CHECK-NEXT: class/Swift | X | s:12index_locals1XC | <no-cgname> | Def - RelChild
// CHECK-NEXT: instance-method/Swift | baz() | s:12index_locals1XC3bazyyF | <no-cgname> | Def,Dyn,RelChild - RelChild,RelCall,RelCont
// CHECK-NEXT: function(local)/Swift | inner(_:) | s:12index_locals1XC3bazyyF5innerL_yS2iF | <no-cgname> | Def,Ref,Call,RelChild,RelCall,RelCont - RelChild,RelCont
// CHECK-NEXT: param(local)/Swift | x | s:12index_locals1XC3bazyyF5innerL_yS2iF1xL_Sivp | <no-cgname> | Def,Ref,Read,RelChild,RelCont -
// CHECK-NEXT: function/acc-get(local)/Swift | getter:arr | s:12index_locals1XC3bazyyF3arrL_SaySiGvg | <no-cgname> | Def,Ref,Call,Impl,RelChild,RelCall,RelAcc,RelCont -
// CHECK-NEXT: variable(local)/Swift | arr | s:12index_locals1XC3bazyyF3arrL_SaySiGvp | <no-cgname> | Def,Ref,Read,Writ,RelChild,RelCont - RelChild,RelAcc,RelCont
// CHECK-NEXT: function/acc-set(local)/Swift | setter:arr | s:12index_locals1XC3bazyyF3arrL_SaySiGvs | <no-cgname> | Def,Ref,Call,Impl,RelChild,RelCall,RelAcc,RelCont -
// CHECK-NEXT: constructor/Swift | init(arrayLiteral:) | s:Sa12arrayLiteralSayxGxd_tcfc | <no-cgname> | Ref,Call,Impl,RelCall,RelCont -
// CHECK-NEXT: static-method/infix-operator/Swift | >(_:_:) | s:Si1goiySbSi_SitFZ | <no-cgname> | Ref,Call,RelCall,RelCont -
// CHECK-NEXT: instance-method/Swift | map(_:) | s:SlsE3mapySayqd__Gqd__7ElementQzKXEKlF | <no-cgname> | Ref,Call,Dyn,RelRec,RelCall,RelCont -
// CHECK-NEXT: struct/Swift | Array | s:Sa | <no-cgname> |  - RelRec
