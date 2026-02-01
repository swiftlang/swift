// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default -suppress-notes
import PointeeOverloads

let lPointee_Const = Pointee_Const()
let _ = lPointee_Const.pointee

var vPointee_Const = Pointee_Const()
let _ = vPointee_Const.pointee
vPointee_Const.pointee = 42  // expected-error {{'pointee' is a get-only property}}

let lPointee_NonConst = Pointee_NonConst()
let _ = lPointee_NonConst.pointee // expected-error {{cannot use mutating getter on immutable value}}

var vPointee_NonConst = Pointee_NonConst()
let _ = vPointee_NonConst.pointee
vPointee_NonConst.pointee = 42

let lPointee_Const_NonConst = Pointee_Const_NonConst()
let _ = lPointee_Const_NonConst.pointee

var vPointee_Const_NonConst = Pointee_Const_NonConst()
let _ = vPointee_Const_NonConst.pointee
vPointee_Const_NonConst.pointee = 42

let lPointee_NonConst_Const = Pointee_NonConst_Const()
let _ = lPointee_NonConst_Const.pointee

var vPointee_NonConst_Const = Pointee_NonConst_Const()
let _ = vPointee_NonConst_Const.pointee
vPointee_NonConst_Const.pointee = 42

let lPointee_NonConst_NonConst = Pointee_NonConst_NonConst()
let _ = lPointee_NonConst_NonConst.pointee

var vPointee_NonConst_NonConst = Pointee_NonConst_NonConst()
let _ = vPointee_NonConst_NonConst.pointee
vPointee_NonConst_NonConst.pointee = 42 // expected-error {{'pointee' is a get-only property}}

let lPointee_Volatile = Pointee_Volatile()
let _ = lPointee_Volatile.pointee // expected-error {{has no member 'pointee'}}

var vPointee_Volatile = Pointee_Volatile()
let _ = vPointee_Volatile.pointee // expected-error {{has no member 'pointee'}}
vPointee_Volatile.pointee = 42 // expected-error {{has no member 'pointee'}}

let lPointee_ConstVolatile = Pointee_ConstVolatile()
let _ = lPointee_ConstVolatile.pointee // expected-error {{has no member 'pointee'}}

var vPointee_ConstVolatile = Pointee_ConstVolatile()
let _ = vPointee_ConstVolatile.pointee // expected-error {{has no member 'pointee'}}
vPointee_ConstVolatile.pointee = 42 // expected-error {{has no member 'pointee'}}

let lPointee_Volatile_Const = Pointee_Volatile_Const()
let _ = lPointee_Volatile_Const.pointee

var vPointee_Volatile_Const = Pointee_Volatile_Const()
let _ = vPointee_Volatile_Const.pointee
vPointee_Volatile_Const.pointee = 42 // expected-error {{'pointee' is a get-only property}}

let lPointee_NonConstGetter = Pointee_NonConstGetter()
let _ = lPointee_NonConstGetter.pointee // expected-error {{cannot use mutating getter on immutable value}}

var vPointee_NonConstGetter = Pointee_NonConstGetter()
let _ = vPointee_NonConstGetter.pointee
vPointee_NonConstGetter.pointee = 42 // expected-error {{'pointee' is a get-only property}}

let lPointee_MutableConst = Pointee_MutableConst()
let _ = lPointee_MutableConst.pointee

var vPointee_MutableConst = Pointee_MutableConst()
let _ = vPointee_MutableConst.pointee
vPointee_MutableConst.pointee = 42

let lPointee_LConst = Pointee_LConst()
let _ = lPointee_LConst.pointee

var vPointee_LConst = Pointee_LConst()
let _ = vPointee_LConst.pointee
vPointee_LConst.pointee = 42 // expected-error {{'pointee' is a get-only property}}

let lPointee_LNonConst = Pointee_LNonConst()
let _ = lPointee_LNonConst.pointee // expected-error {{cannot use mutating getter on immutable value}}

var vPointee_LNonConst = Pointee_LNonConst()
let _ = vPointee_LNonConst.pointee
vPointee_LNonConst.pointee = 42

let lPointee_LConst_LNonConst = Pointee_LConst_LNonConst()
let _ = lPointee_LConst_LNonConst.pointee

var vPointee_LConst_LNonConst = Pointee_LConst_LNonConst()
let _ = vPointee_LConst_LNonConst.pointee
vPointee_LConst_LNonConst.pointee = 42

let lPointee_LNonConst_LConst = Pointee_LNonConst_LConst()
let _ = lPointee_LNonConst_LConst.pointee

var vPointee_LNonConst_LConst = Pointee_LNonConst_LConst()
let _ = vPointee_LNonConst_LConst.pointee
vPointee_LNonConst_LConst.pointee = 42

let lPointee_LConst_RConst = Pointee_LConst_RConst()
let _ = lPointee_LConst_RConst.pointee

var vPointee_LConst_RConst = Pointee_LConst_RConst()
let _ = vPointee_LConst_RConst.pointee
vPointee_LConst_RConst.pointee = 42 // expected-error {{'pointee' is a get-only property}}

let lPointee_LNonConst_RNonConst = Pointee_LNonConst_RNonConst()
let _ = lPointee_LNonConst_RNonConst.pointee // expected-error {{cannot use mutating getter on immutable value}}

var vPointee_LNonConst_RNonConst = Pointee_LNonConst_RNonConst()
let _ = vPointee_LNonConst_RNonConst.pointee
vPointee_LNonConst_RNonConst.pointee = 42
