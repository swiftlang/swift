// RUN: %target-typecheck-verify-swift -suppress-notes -I %S/Inputs -cxx-interoperability-mode=default
// RUN: %target-typecheck-verify-swift -suppress-notes -I %S/Inputs -cxx-interoperability-mode=default -Xcc -std=c++23 -D CXX23 -verify-additional-prefix CXX23-

import SubscriptOverloads

let u = Overloaded()
var v = Overloaded()

let val = Num(123)

let signed: CInt = 0
let unsigned: CUnsignedInt = 0
let num = Num(0)
let float: Float = 4.2
let unit = Overloaded.Unit()

let _: Num = v[signed]
v[signed] = val // expected-error {{no exact matches in call to subscript}}

let _: Num = u[signed]
u[signed] = val // expected-error {{subscript is get-only}}

let _: Num = v[unsigned]
v[unsigned] = val

let _: Num = u[unsigned]
u[unsigned] = val // expected-error {{'u' is a 'let' constant}}

let _: Num = v[num]
v[num] = val

let _: Num = u[num] // expected-error {{cannot use mutating getter on immutable value}}
u[num] = val // expected-error {{cannot convert value of type 'Num' to expected argument type}}
             // FIXME: ^this should complain about 'u' being a 'let' constant


let _: Float = v[float]
v[float] = val // expected-error {{no exact matches in call to subscript}}
               // FIXME: ^this should complain about 'u' being a 'let' constant

let _: Float = u[float]
u[float] = val // expected-error {{no exact matches in call to subscript}}
               // FIXME: ^this should complain about 'u' being a 'let' constant

let _: Overloaded.Unit = v[unit]
v[unit] = val // expected-error {{cannot assign value of type 'Num' to subscript of type 'Overloaded.Unit'}}

v[unit] = unit

let _: Overloaded.Unit = u[unit] // expected-error {{cannot use mutating getter on immutable value: 'u' is a 'let' constant}}
u[unit] = val // expected-error {{cannot convert value of type 'Overloaded.Unit' to expected argument type}}

u[unit] = unit // expected-error {{failed to produce diagnostic}}
               // FIXME: ^this should complain about 'u' being a 'let' constant

let _: Bool = v[Overloaded.Bogus()]
v[Overloaded.Bogus()] = false // expected-error {{subscript is get-only}}
let _: Bool = u[Overloaded.Bogus()]
u[Overloaded.Bogus()] = false // expected-error {{subscript is get-only}}

var CharPtr = v[Overloaded.GetCharPtr()]
var ConstCharPtr = v[Overloaded.GetConstCharPtr()]

CharPtr = v[CharPtr]
CharPtr = u[CharPtr] // expected-error {{cannot use mutating getter on immutable value}}
                     // FIXME: ^ this should be allowed
ConstCharPtr = v[ConstCharPtr]
ConstCharPtr = u[ConstCharPtr]

#if CXX23
let _: Bool = v[]
v[] = true // expected-CXX23-error {{subscript is get-only}}
let _: Bool = u[]
u[] = true // expected-CXX23-error {{subscript is get-only}}

let _: Num = v[1.0, 2.0]
v[1.0, 2.0] = val // expected-CXX23-error {{subscript is get-only}}
let _: Num = u[1.0, 2.0]
u[1.0, 2.0] = val // expected-CXX23-error {{subscript is get-only}}

let _: CInt = v[1, 2]
v[1, 2] = 12 // expected-CXX23-error {{subscript is get-only}}
let _: CInt = u[1, 2]
u[1, 2] = 12 // expected-CXX23-error {{subscript is get-only}}
#endif


let getPtr = Overloaded.GetPtr(index: 0)
let getRef = Overloaded.GetRef(index: 0)
let getPtrRef = Overloaded.GetPtrRef(index: 0)
let getRefVal = Overloaded.GetRefVal(index: 0)
let getValPtr = Overloaded.GetValPtr(index: 0)

let vGetPtr: CUnsignedInt = v[getPtr]
v[getPtr] = vGetPtr
let uGetPtr: CUnsignedInt = u[getPtr] // expected-error {{cannot use mutating getter on immutable value: 'u' is a 'let' constant}}
u[getPtr] = uGetPtr // expected-error {{no exact matches in call to subscript}}
                    // FIXME: ^this should complain about 'u' being a 'let' constant

let vGetRef: UnsafeMutablePointer<UInt32>? = v[getRef]
// FIXME: ^why is this returning a mutable pointer?
v[getRef] = vGetRef
let uGetRef: UnsafeMutablePointer<UInt32>? = u[getRef] // expected-error {{cannot use mutating getter on immutable value: 'u' is a 'let' constant}}
                                                       // FIXME: ^this should complain about 'u' being a 'let' constant
u[getRef] = uGetRef // expected-error {{failed to produce diagnostic for expression}}
                    // FIXME: ^this should complain about 'u' being a 'let' constant

let vGetPtrRef: CUnsignedInt = v[getPtrRef]
v[getPtrRef] = vGetPtrRef
let uGetPtrRef: CUnsignedInt = u[getPtrRef]
u[getPtrRef] = uGetPtrRef // expected-error {{'u' is a 'let' constant}}

let vGetRefVal: CUnsignedInt = v[getRefVal]
v[getRefVal] = vGetRefVal
let uGetRefVal: CUnsignedInt = u[getRefVal]
u[getRefVal] = uGetRefVal // expected-error {{'u' is a 'let' constant}}

let vGetValPtr: UnsafePointer<UInt32>? = v[getValPtr]
v[getValPtr] = vGetValPtr // expected-error {{subscript is get-only}}
let uGetValPtr: UnsafePointer<UInt32>? = u[getValPtr]
u[getValPtr] = uGetValPtr // expected-error {{subscript is get-only}}

let deprecatedIdx = Overloaded.DeprecatedIndex(index: 0)
let _: CUnsignedInt = v[deprecatedIdx] // expected-warning {{deprecated: use GetVal instead}}
