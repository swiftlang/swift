; RUN: swift-demangle `cat %s.in` | FileCheck %s

; CHECK: A32Si ---> swift.Int64[32]
; CHECK: Bf80_ ---> Builtin.Float80
; CHECK: Bi32_ ---> Builtin.Int32
; CHECK: BO ---> Builtin.ObjCPointer
; CHECK: Bo ---> Builtin.ObjectPointer
; CHECK: Bp ---> Builtin.RawPointer
; CHECK: Bu ---> Builtin.OpaquePointer
; CHECK: Bv4Bi8_ ---> Builtin.Vec4xInt8
; CHECK: Bv4Bf16_ ---> Builtin.Vec4xFloat16
; CHECK: Bv4Bp ---> Builtin.Vec4xRawPointer
; CHECK: Sa ---> swift.Slice
; CHECK: Sb ---> swift.Bool
; CHECK: Sc ---> swift.Char
; CHECK: Sd ---> swift.Float64
; CHECK: Sf ---> swift.Float32
; CHECK: Si ---> swift.Int64
; CHECK: SS ---> swift.String
; CHECK: Su ---> swift.UInt64
; CHECK: GSaSS_ ---> swift.Slice<swift.String>
; CHECK: GCSs10DictionarySSSi_ ---> swift.Dictionary<swift.String, swift.Int64>
; CHECK: VSs7CString ---> swift.CString
; CHECK: CSo8NSObject ---> ObjectiveC.NSObject
; CHECK: O6Monads6Either ---> Monads.Either
; CHECK: bSiSu ---> [objc_block] (swift.Int64) -> swift.UInt64
; CHECK: bTSiSc_Su ---> [objc_block] (swift.Int64, swift.Char) -> swift.UInt64
; CHECK: FSiSu ---> (swift.Int64) -> swift.UInt64
; CHECK: fSiFScSu ---> (swift.Int64)(swift.Char) -> swift.UInt64
; CHECK: FSiFScSu ---> (swift.Int64) -> (swift.Char) -> swift.UInt64
; CHECK: MSi ---> swift.Int64.metatype
; CHECK: P_ ---> protocol<>
; CHECK: P3foo3bar_ ---> foo.bar
; CHECK: P3foo3barS_3bas_ ---> protocol<foo.bar, bas>
; CHECK: TP3foo3barS_3bas_PS1__PS1_S_3zimS0___ ---> (protocol<foo.bar, bas>, bas, protocol<bas, zim, foo.bar>)
; CHECK: RSi ---> [byref] swift.Int64
; CHECK: TSiSu_ ---> (swift.Int64, swift.UInt64)
; CHECK: tSiSu_ ---> (swift.Int64, swift.UInt64...)
; CHECK: U___FQ_U____FQ_T_ ---> <A, B>(A) -> <C, D, E>(C) -> ()
; CHECK: U___FQ_U____FQ0_T_ ---> <A, B>(A) -> <C, D, E>(D) -> ()
; CHECK: U___FQ_U____FQ1_T_ ---> <A, B>(A) -> <C, D, E>(E) -> ()
; CHECK: U___FQ_U____FQd__T_ ---> <A, B>(A) -> <C, D, E>(A) -> ()
; CHECK: U___FQ_U____FQd_0_T_ ---> <A, B>(A) -> <C, D, E>(B) -> ()
; CHECK: _T3foo3barSi ---> foo.bar : swift.Int64
; CHECK: _T3foo3barSia ---> foo.bar : swift.Int64 addressor
; CHECK: _T3foo3barSig ---> foo.bar : swift.Int64 getter
; CHECK: _T3foo3barSis ---> foo.bar : swift.Int64 setter
; CHECK: _TC3foo3bar3basfS0_FT3zimCS_3zim_T_ ---> foo.bar.bas : (foo.bar)(zim : foo.zim) -> ()
; CHECK: _TToC3foo3bar3basfS0_FT3zimCS_3zim_T_ ---> [objc] foo.bar.bas : (foo.bar)(zim : foo.zim) -> ()
; CHECK: _Tnk_C3foo3bar3basfS0_FT3zimCS_3zim_T_ ---> protocol witness for foo.bar.bas : (foo.bar)(zim : foo.zim) -> ()
; CHECK: _TLC3foo3bar3basfS0_FT3zimCS_3zim_T_ ---> local foo.bar.bas : (foo.bar)(zim : foo.zim) -> ()
; CHECK: _T3foooi1pFTCS_3barVS_3bas_OS_3zim ---> foo.+ [infix] : (foo.bar, foo.bas) -> foo.zim
; CHECK: _T3foooP1xFTCS_3barVS_3bas_OS_3zim ---> foo.^ [postfix] : (foo.bar, foo.bas) -> foo.zim
; CHECK: _TC3foo3barCfMS0_FT_S0_ ---> foo.bar.constructor : (foo.bar.metatype)() -> foo.bar
; CHECK: _TC3foo3barcfMS0_FT_S0_ ---> foo.bar.constructor [initializing] : (foo.bar.metatype)() -> foo.bar
; CHECK: _TC3foo3barD ---> foo.bar.destructor
; CHECK: _TC3foo3bard ---> foo.bar.destructor [destroying]
; CHECK: _TMPdC3foo3bar ---> direct generic type metadata pattern for foo.bar
; CHECK: _TMPiC3foo3bar ---> indirect generic type metadata pattern for foo.bar
; CHECK: _TMmC3foo3bar ---> metaclass for foo.bar
; CHECK: _TMdC3foo3bar ---> direct  type metadata for foo.bar
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
; CHECK: _TWoC3foo3bar3basFSiSi ---> witness table Offset for foo.bar.bas : (swift.Int64) -> swift.Int64
; CHECK: _TWvdC3foo3bar3basSi ---> direct field Offset for foo.bar.bas : swift.Int64
; CHECK: _TWviC3foo3bar3basSi ---> indirect field Offset for foo.bar.bas : swift.Int64
; CHECK: _TTbbSiSi ---> bridge-to-block std::function for [objc_block] (swift.Int64) -> swift.Int64
; CHECK: _TSC5greenVSC5Colorg ---> C.green : C.Color getter
; CHECK: _T1t1fFT1iSi1sSS_T_e_ ---> t.f : (i : swift.Int64, s : swift.String) -> ()
; CHECK: _T1t1fFT1iSi1sSS_T_e0_ ---> t.f : (i : swift.Int64, s : swift.String) -> ()
; CHECK: ZZ ---> ZZ
; CHECK: B ---> B
; CHECK: BSi ---> BSi
; CHECK: Bx ---> Bx
; CHECK: C ---> C
; CHECK: T ---> T
; CHECK: TSi ---> TSi
; CHECK: Qd_ ---> Qd_
; CHECK: U__FQo_Si ---> U__FQo_Si
; CHECK: U__FQD__Si ---> U__FQD__Si
; CHECK: U___FQ_U____FQd0__T_ ---> U___FQ_U____FQd0__T_
; CHECK: U___FQ_U____FQd_1_T_ ---> U___FQ_U____FQd_1_T_
; CHECK: U___FQ_U____FQ2_T_ ---> U___FQ_U____FQ2_T_
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
