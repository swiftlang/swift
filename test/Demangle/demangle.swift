; RUN: %swift-demangle `cat %s.in` | FileCheck %s

; CHECK: _TtA32Si ---> swift.Int64[32]
; CHECK: _TtBf80_ ---> Builtin.Float80
; CHECK: _TtBi32_ ---> Builtin.Int32
; CHECK: _TtBO ---> Builtin.ObjCPointer
; CHECK: _TtBo ---> Builtin.ObjectPointer
; CHECK: _TtBp ---> Builtin.RawPointer
; CHECK: _TtBv4Bi8_ ---> Builtin.Vec4xInt8
; CHECK: _TtBv4Bf16_ ---> Builtin.Vec4xFloat16
; CHECK: _TtBv4Bp ---> Builtin.Vec4xRawPointer
; CHECK: _TtSa ---> swift.Slice
; CHECK: _TtSb ---> swift.Bool
; CHECK: _TtSc ---> swift.Char
; CHECK: _TtSd ---> swift.Float64
; CHECK: _TtSf ---> swift.Float32
; CHECK: _TtSi ---> swift.Int64
; CHECK: _TtSq ---> swift.Optional
; CHECK: _TtSS ---> swift.String
; CHECK: _TtSu ---> swift.UInt64
; CHECK: _TtGSaSS_ ---> swift.String[]
; CHECK: _TtGSqSS_ ---> swift.String?
; CHECK: _TtGCSs10DictionarySSSi_ ---> swift.Dictionary<swift.String, swift.Int64>
; CHECK: _TtVSs7CString ---> swift.CString
; CHECK: _TtCSo8NSObject ---> ObjectiveC.NSObject
; CHECK: _TtO6Monads6Either ---> Monads.Either
; CHECK: _TtbSiSu ---> @objc_block (swift.Int64) -> swift.UInt64
; CHECK: _TtbTSiSc_Su ---> @objc_block (swift.Int64, swift.Char) -> swift.UInt64
; CHECK: _TtFSiSu ---> (swift.Int64) -> swift.UInt64
; CHECK: _TtfSiFScSu ---> (swift.Int64)(swift.Char) -> swift.UInt64
; CHECK: _TtFSiFScSu ---> (swift.Int64) -> (swift.Char) -> swift.UInt64
; CHECK: _TtMSi ---> swift.Int64.metatype
; CHECK: _TtP_ ---> protocol<>
; CHECK: _TtP3foo3bar_ ---> foo.bar
; CHECK: _TtP3foo3barS_3bas_ ---> protocol<foo.bar, foo.bas>
; CHECK: _TtTP3foo3barS_3bas_PS1__PS1_S_3zimS0___ ---> (protocol<foo.bar, foo.bas>, foo.bas, protocol<foo.bas, foo.zim, foo.bar>)
; CHECK: _TtRSi ---> @inout swift.Int64
; CHECK: _TtTSiSu_ ---> (swift.Int64, swift.UInt64)
; CHECK: _TttSiSu_ ---> (swift.Int64, swift.UInt64...)
; CHECK: _TtT3fooSi3barSu_ ---> (foo : swift.Int64, bar : swift.UInt64)
; CHECK: _TtU__FQ_Si ---> <A>(A) -> swift.Int64
; CHECK: _TtU3foo3bar_S_3basS_3zim__FTQ0_Q__Si ---> <A : foo.bar, B : protocol<foo.bas, foo.zim>>(B, A) -> swift.Int64
; CHECK: _TtU3foo3bar_S_3basS_3zim___FTQ0_Q__Q1_ ---> <A : foo.bar, B : protocol<foo.bas, foo.zim>, C>(B, A) -> C
; CHECK: _TtU___FQ_U____FQ_T_ ---> <A, B>(A) -> <C, D, E>(C) -> ()
; CHECK: _TtU___FQ_U____FQ0_T_ ---> <A, B>(A) -> <C, D, E>(D) -> ()
; CHECK: _TtU___FQ_U____FQ1_T_ ---> <A, B>(A) -> <C, D, E>(E) -> ()
; CHECK: _TtU___FQ_U____FQd__T_ ---> <A, B>(A) -> <C, D, E>(A) -> ()
; CHECK: _TtU___FQ_U____FQd_0_T_ ---> <A, B>(A) -> <C, D, E>(B) -> ()
; CHECK: _T3foo3barSi ---> foo.bar : swift.Int64
; CHECK: _T3foo3barSia ---> foo.bar.addressor : swift.Int64
; CHECK: _T3foo3barSig ---> foo.bar.getter : swift.Int64
; CHECK: _T3foo3barSis ---> foo.bar.setter : swift.Int64
; CHECK: _TC3foo3bar3basfS0_FT3zimCS_3zim_T_ ---> foo.bar.bas (foo.bar)(zim : foo.zim) -> ()
; CHECK: _TToC3foo3bar3basfS0_FT3zimCS_3zim_T_ ---> [objc] foo.bar.bas (foo.bar)(zim : foo.zim) -> ()
; CHECK: _Tnk_C3foo3bar3basfS0_FT3zimCS_3zim_T_ ---> protocol witness for foo.bar.bas (foo.bar)(zim : foo.zim) -> ()
; CHECK: _TLC3foo3bar3basfS0_FT3zimCS_3zim_T_ ---> local foo.bar.bas (foo.bar)(zim : foo.zim) -> ()
; CHECK: _T3foooi1pFTCS_3barVS_3bas_OS_3zim ---> foo.+ [infix] (foo.bar, foo.bas) -> foo.zim
; CHECK: _T3foooP1xFTCS_3barVS_3bas_OS_3zim ---> foo.^ [postfix] (foo.bar, foo.bas) -> foo.zim
; CHECK: _TC3foo3barCfMS0_FT_S0_ ---> foo.bar.__allocating_init (foo.bar.metatype)() -> foo.bar
; CHECK: _TC3foo3barcfMS0_FT_S0_ ---> foo.bar.init (foo.bar.metatype)() -> foo.bar
; CHECK: _TC3foo3barD ---> foo.bar.__deallocating_destructor
; CHECK: _TC3foo3bard ---> foo.bar.destructor
; CHECK: _TMPdC3foo3bar ---> direct generic type metadata pattern for foo.bar
; CHECK: _TMPiC3foo3bar ---> indirect generic type metadata pattern for foo.bar
; CHECK: _TMnC3foo3bar ---> nominal type descriptor for foo.bar
; CHECK: _TMmC3foo3bar ---> metaclass for foo.bar
; CHECK: _TMdC3foo3bar ---> direct type metadata for foo.bar
; CHECK: _TwalC3foo3bar ---> allocateBuffer value witness for foo.bar
; CHECK: _TwcaC3foo3bar ---> assignWithCopy value witness for foo.bar
; CHECK: _TwtaC3foo3bar ---> assignWithTake value witness for foo.bar
; CHECK: _TwdeC3foo3bar ---> deallocateBuffer value witness for foo.bar
; CHECK: _TwxxC3foo3bar ---> destroy value witness for foo.bar
; CHECK: _TwXXC3foo3bar ---> destroyBuffer value witness for foo.bar
; CHECK: _TwCPC3foo3bar ---> initializeBufferWithCopyOfBuffer value witness for foo.bar
; CHECK: _TwCpC3foo3bar ---> initializeBufferWithCopy value witness for foo.bar
; CHECK: _TwcpC3foo3bar ---> initializeWithCopy value witness for foo.bar
; CHECK: _TwTkC3foo3bar ---> initializeBufferWithTake value witness for foo.bar
; CHECK: _TwtkC3foo3bar ---> initializeWithTake value witness for foo.bar
; CHECK: _TwprC3foo3bar ---> projectBuffer value witness for foo.bar
; CHECK: _TwtyC3foo3bar ---> typeof value witness for foo.bar
; CHECK: _TWVC3foo3bar ---> value witness table for foo.bar
; CHECK: _TWoC3foo3bar3basFSiSi ---> witness table offset for foo.bar.bas (swift.Int64) -> swift.Int64
; CHECK: _TWvdC3foo3bar3basSi ---> direct field offset for foo.bar.bas : swift.Int64
; CHECK: _TWviC3foo3bar3basSi ---> indirect field offset for foo.bar.bas : swift.Int64
; CHECK: _TWPC3foo3barS_8barrableS_ ---> protocol witness table for foo.bar : foo.barrable in foo
; CHECK: _TWZC3foo3barS_8barrableS_ ---> lazy protocol witness table accessor for foo.bar : foo.barrable in foo
; CHECK: _TWzC3foo3barS_8barrableS_ ---> lazy protocol witness table template for foo.bar : foo.barrable in foo
; CHECK: _TWDC3foo3barS_8barrableS_ ---> dependent protocol witness table generator for foo.bar : foo.barrable in foo
; CHECK: _TWdC3foo3barS_8barrableS_ ---> dependent protocol witness table template for foo.bar : foo.barrable in foo
; CHECK: _TTbbSiSi ---> bridge-to-block function for @objc_block (swift.Int64) -> swift.Int64
; CHECK: _TSC5greenVSC5Colorg ---> C.green.getter : C.Color
; CHECK: _T1t1fFT1iSi1sSS_T_e_ ---> t.f (i : swift.Int64, s : swift.String) -> ()
; CHECK: _T1t1fFT1iSi1sSS_T_e0_ ---> t.f (i : swift.Int64, s : swift.String) -> ()
; CHECK: _TSqCU__fMGSqQ__FT_GSqQ__ ---> swift.Optional.init <A>(A?.metatype)() -> A?
; CHECK: _T21class_bound_protocols32class_bound_protocol_compositionFT1xPS_10ClassBoundS_13NotClassBound__PS0_S1__ ---> class_bound_protocols.class_bound_protocol_composition (x : protocol<class_bound_protocols.ClassBound, class_bound_protocols.NotClassBound>) -> protocol<class_bound_protocols.ClassBound, class_bound_protocols.NotClassBound>
; CHECK: _TtZZ ---> _TtZZ
; CHECK: _TtB ---> _TtB
; CHECK: _TtBSi ---> _TtBSi
; CHECK: _TtBx ---> _TtBx
; CHECK: _TtC ---> _TtC
; CHECK: _TtT ---> _TtT
; CHECK: _TtTSi ---> _TtTSi
; CHECK: _TtQd_ ---> _TtQd_
; CHECK: _TtU__FQo_Si ---> _TtU__FQo_Si
; CHECK: _TtU__FQD__Si ---> _TtU__FQD__Si
; CHECK: _TtU___FQ_U____FQd0__T_ ---> _TtU___FQ_U____FQd0__T_
; CHECK: _TtU___FQ_U____FQd_1_T_ ---> _TtU___FQ_U____FQd_1_T_
; CHECK: _TtU___FQ_U____FQ2_T_ ---> _TtU___FQ_U____FQ2_T_
; CHECK: _Tw ---> _Tw
; CHECK: _TWa ---> _TWa
; CHECK: _Twal ---> _Twal
; CHECK: _T ---> _T
; CHECK: _TTo ---> _TTo
; CHECK: _TC ---> _TC
; CHECK: _TM ---> _TM
; CHECK: _TMd ---> _TMd
; CHECK: _TW ---> _TW
; CHECK: _TWV ---> _TWV
; CHECK: _TWo ---> _TWo
; CHECK: _TWv ---> _TWv
; CHECK: _TWvd ---> _TWvd
; CHECK: _TWvi ---> _TWvi
; CHECK: _TWvx ---> _TWvx
; CHECK: _TtVCC4main3Foo4Ding3Str ---> main.Foo.Ding.Str
; CHECK: _TVCC6nested6AClass12AnotherClass7AStruct9aFunctionfRS2_FT1aSi_S2_ ---> nested.AClass.AnotherClass.AStruct.aFunction (@inout nested.AClass.AnotherClass.AStruct)(a : swift.Int64) -> nested.AClass.AnotherClass.AStruct
; CHECK: _T3foo3barU3foo8Barrable__FQ_QQ_3Bar ---> foo.bar <A : foo.Barrable>(A) -> A.Bar
; CHECK: _TtXwC10attributes10SwiftClass ---> [weak] attributes.SwiftClass
; CHECK: _TtXoC10attributes10SwiftClass ---> [unowned] attributes.SwiftClass
; CHECK: _TtERR ---> <ERROR TYPE>
; CHECK: _TtGSqGSaC5sugar7MyClass__ ---> (sugar.MyClass[])?
; CHECK: _TtGSaGSqC5sugar7MyClass__ ---> (sugar.MyClass?)[]
