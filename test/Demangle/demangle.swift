; This is not really a Swift source file: -*- Text -*-
; RUN: swift-demangle `sed -ne '/C\HECK:/s/^.*C\HECK: \(.*\) --->.*/\1/p' < %s` | FileCheck %s

; RUN: sed -ne '/C\HECK:/s/^\(.*C\HECK: \).*\( ---> \)\(.*\)/\1\3\2\3/p' < %s > %t
; RUN: sed -ne '/C\HECK:/p' < %s | swift-demangle | diff %t -

; RUN: swift-demangle __TtSi | FileCheck %s -check-prefix=DOUBLE
; DOUBLE: _TtSi ---> Swift.Int

; CHECK: _TtBf80_ ---> Builtin.Float80
; CHECK: _TtBi32_ ---> Builtin.Int32
; CHECK: _TtBw ---> Builtin.Word
; CHECK: _TtBO ---> Builtin.UnknownObject
; CHECK: _TtBo ---> Builtin.NativeObject
; CHECK: _TtBp ---> Builtin.RawPointer
; CHECK: _TtBv4Bi8_ ---> Builtin.Vec4xInt8
; CHECK: _TtBv4Bf16_ ---> Builtin.Vec4xFloat16
; CHECK: _TtBv4Bp ---> Builtin.Vec4xRawPointer
; CHECK: _TtSa ---> Swift.Array
; CHECK: _TtSb ---> Swift.Bool
; CHECK: _TtSc ---> Swift.UnicodeScalar
; CHECK: _TtSd ---> Swift.Double
; CHECK: _TtSf ---> Swift.Float
; CHECK: _TtSi ---> Swift.Int
; CHECK: _TtSq ---> Swift.Optional
; CHECK: _TtSS ---> Swift.String
; CHECK: _TtSu ---> Swift.UInt
; CHECK: _TtGSaSS_ ---> [Swift.String]
; CHECK: _TtGSqSS_ ---> Swift.String?
; CHECK: _TtGSQSS_ ---> Swift.String!
; CHECK: _TtGVSs10DictionarySSSi_ ---> [Swift.String : Swift.Int]
; CHECK: _TtVSs7CString ---> Swift.CString
; CHECK: _TtCSo8NSObject ---> ObjectiveC.NSObject
; CHECK: _TtO6Monads6Either ---> Monads.Either
; CHECK: _TtbSiSu ---> @objc_block (Swift.Int) -> Swift.UInt
; CHECK: _TtbTSiSc_Su ---> @objc_block (Swift.Int, Swift.UnicodeScalar) -> Swift.UInt
; CHECK: _TtFSiSu ---> (Swift.Int) -> Swift.UInt
; CHECK: _TtKSiSu ---> @autoclosure (Swift.Int) -> Swift.UInt
; CHECK: _TtfSiFScSu ---> (Swift.Int)(Swift.UnicodeScalar) -> Swift.UInt
; CHECK: _TtFSiFScSu ---> (Swift.Int) -> (Swift.UnicodeScalar) -> Swift.UInt
; CHECK: _TtMSi ---> Swift.Int.Type
; CHECK: _TtP_ ---> protocol<>
; CHECK: _TtP3foo3bar_ ---> foo.bar
; CHECK: _TtP3foo3barS_3bas_ ---> protocol<foo.bar, foo.bas>
; CHECK: _TtTP3foo3barS_3bas_PS1__PS1_S_3zimS0___ ---> (protocol<foo.bar, foo.bas>, foo.bas, protocol<foo.bas, foo.zim, foo.bar>)
; CHECK: _TtRSi ---> inout Swift.Int
; CHECK: _TtTSiSu_ ---> (Swift.Int, Swift.UInt)
; CHECK: _TttSiSu_ ---> (Swift.Int, Swift.UInt...)
; CHECK: _TtT3fooSi3barSu_ ---> (foo : Swift.Int, bar : Swift.UInt)
; CHECK: _TtU__FQ_Si ---> <A>(A) -> Swift.Int
; CHECK: _TtU3foo3bar_S_3basS_3zim__FTQ0_Q__Si ---> <A : foo.bar, B : protocol<foo.bas, foo.zim>>(B, A) -> Swift.Int
; CHECK: _TtU3foo3bar_S_3basS_3zim___FTQ0_Q__Q1_ ---> <A : foo.bar, B : protocol<foo.bas, foo.zim>, C>(B, A) -> C
; CHECK: _TtU3foo3bar_S_3basS_3zim__US_4zang__FTQ0_Q__Q1_ ---> <A : foo.bar, B : protocol<foo.bas, foo.zim>, C>(B, A) -> C
; CHECK: _TtU3foo3bar_S_3basS_3zim___US_4zang___FTQ0_Q__Q1_ ---> <A : foo.bar, B : protocol<foo.bas, foo.zim>, C, D>(B, A) -> C
; CHECK: _TtU___FQ_U____FQ_T_ ---> <A, B>(A) -> <C, D, E>(C) -> ()
; CHECK: _TtU___FQ_U____FQ0_T_ ---> <A, B>(A) -> <C, D, E>(D) -> ()
; CHECK: _TtU___FQ_U____FQ1_T_ ---> <A, B>(A) -> <C, D, E>(E) -> ()
; CHECK: _TtU___FQ_U____FQd__T_ ---> <A, B>(A) -> <C, D, E>(A) -> ()
; CHECK: _TtU___FQ_U____FQd_0_T_ ---> <A, B>(A) -> <C, D, E>(B) -> ()
; CHECK: _TtU___FQ_U____FQd_0_T__ASD_foo ---> <A, B>(A) -> <C, D, E>(B) -> () with unmangled suffix "_ASD_foo"
; CHECK: _Ttu0_R_Fq_q_ ---> <T_0_0> (T_0_0) -> T_0_0
; CHECK: _Ttu0_0_R_Fq_qd__ ---> <T_0_0, T_1_0> (T_0_0) -> T_1_0
; CHECK: _Ttu1_R_Fq_q0_ ---> <T_0_0, T_0_1> (T_0_0) -> T_0_1
; CHECK: _Ttu0_RPq_PSs8Runcible__Fq_qq_5Mince ---> <T_0_0 where T_0_0: Swift.Runcible> (T_0_0) -> T_0_0.Mince
; CHECK: _Ttu0_RPq_PSs8Runcible_Eqq_5Minceq__Fq_q_ ---> <T_0_0 where T_0_0: Swift.Runcible, T_0_0.Mince == T_0_0> (T_0_0) -> T_0_0
; CHECK: _Tv3foo3barSi ---> foo.bar : Swift.Int
; CHECK: _TF3fooau3barSi ---> foo.bar.unsafeMutableAddressor : Swift.Int
; CHECK: _TF3foolu3barSi ---> foo.bar.unsafeAddressor : Swift.Int
; CHECK: _TF3fooaO3barSi ---> foo.bar.owningMutableAddressor : Swift.Int
; CHECK: _TF3foolO3barSi ---> foo.bar.owningAddressor : Swift.Int
; CHECK: _TF3fooao3barSi ---> foo.bar.nativeOwningMutableAddressor : Swift.Int
; CHECK: _TF3foolo3barSi ---> foo.bar.nativeOwningAddressor : Swift.Int
; CHECK: _TF3fooap3barSi ---> foo.bar.nativePinningMutableAddressor : Swift.Int
; CHECK: _TF3foolp3barSi ---> foo.bar.nativePinningAddressor : Swift.Int
; CHECK: _TF3foog3barSi ---> foo.bar.getter : Swift.Int
; CHECK: _TF3foos3barSi ---> foo.bar.setter : Swift.Int
; CHECK: _TFC3foo3bar3basfS0_FT3zimCS_3zim_T_ ---> foo.bar.bas (foo.bar)(zim : foo.zim) -> ()
; CHECK: _TToFC3foo3bar3basfS0_FT3zimCS_3zim_T_ ---> @objc foo.bar.bas (foo.bar)(zim : foo.zim) -> ()
; CHECK: _TTDFC3foo3bar3basfS0_FT3zimCS_3zim_T_ ---> dynamic foo.bar.bas (foo.bar)(zim : foo.zim) -> ()
; CHECK: _TFC3foo3bar3basfS0_FT3zimCS_3zim_T_ ---> foo.bar.bas (foo.bar)(zim : foo.zim) -> ()
; CHECK: _TF3foooi1pFTCS_3barVS_3bas_OS_3zim ---> foo.+ infix (foo.bar, foo.bas) -> foo.zim
; CHECK: _TF3foooP1xFTCS_3barVS_3bas_OS_3zim ---> foo.^ postfix (foo.bar, foo.bas) -> foo.zim
; CHECK: _TFC3foo3barCfMS0_FT_S0_ ---> foo.bar.__allocating_init (foo.bar.Type)() -> foo.bar
; CHECK: _TFC3foo3barcfMS0_FT_S0_ ---> foo.bar.init (foo.bar.Type)() -> foo.bar
; CHECK: _TFC3foo3barD ---> foo.bar.__deallocating_deinit
; CHECK: _TFC3foo3bard ---> foo.bar.deinit
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
; CHECK: _TwTKC3foo3bar ---> initializeBufferWithTakeOfBuffer value witness for foo.bar
; CHECK: _TwTkC3foo3bar ---> initializeBufferWithTake value witness for foo.bar
; CHECK: _TwtkC3foo3bar ---> initializeWithTake value witness for foo.bar
; CHECK: _TwprC3foo3bar ---> projectBuffer value witness for foo.bar
; CHECK: _TWVC3foo3bar ---> value witness table for foo.bar
; CHECK: _TWoFC3foo3bar3basFSiSi ---> witness table offset for foo.bar.bas (Swift.Int) -> Swift.Int
; CHECK: _TWvdvC3foo3bar3basSi ---> direct field offset for foo.bar.bas : Swift.Int
; CHECK: _TWvivC3foo3bar3basSi ---> indirect field offset for foo.bar.bas : Swift.Int
; CHECK: _TWPC3foo3barS_8barrableSs ---> protocol witness table for foo.bar : foo.barrable in Swift
; CHECK: _TWZC3foo3barS_8barrableS_ ---> lazy protocol witness table accessor for foo.bar : foo.barrable in foo
; CHECK: _TWzC3foo3barS_8barrableS_ ---> lazy protocol witness table template for foo.bar : foo.barrable in foo
; CHECK: _TWDC3foo3barS_8barrableS_ ---> dependent protocol witness table generator for foo.bar : foo.barrable in foo
; CHECK: _TWdC3foo3barS_8barrableS_ ---> dependent protocol witness table template for foo.bar : foo.barrable in foo
; CHECK: _TFSCg5greenVSC5Color ---> C.green.getter : C.Color
; CHECK: _TIF1t1fFT1iSi1sSS_T_A_ ---> t.(f (i : Swift.Int, s : Swift.String) -> ()).(default argument 0)
; CHECK: _TIF1t1fFT1iSi1sSS_T_A0_ ---> t.(f (i : Swift.Int, s : Swift.String) -> ()).(default argument 1)
; CHECK: _TFSqCU__fMGSqQ__FT_GSqQ__ ---> Swift.Optional.init <A>(A?.Type)() -> A?
; CHECK: _TF21class_bound_protocols32class_bound_protocol_compositionFT1xPS_10ClassBoundS_13NotClassBound__PS0_S1__ ---> class_bound_protocols.class_bound_protocol_composition (x : protocol<class_bound_protocols.ClassBound, class_bound_protocols.NotClassBound>) -> protocol<class_bound_protocols.ClassBound, class_bound_protocols.NotClassBound>
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
; CHECK: _TFVCC6nested6AClass12AnotherClass7AStruct9aFunctionfRS2_FT1aSi_S2_ ---> nested.AClass.AnotherClass.AStruct.aFunction (inout nested.AClass.AnotherClass.AStruct)(a : Swift.Int) -> nested.AClass.AnotherClass.AStruct
; CHECK: _TF3foo3barU3foo8Barrable__FQ_QQ_3Bar ---> foo.bar <A : foo.Barrable>(A) -> A.Bar
; CHECK: _TtXwC10attributes10SwiftClass ---> weak attributes.SwiftClass
; CHECK: _TtXoC10attributes10SwiftClass ---> unowned attributes.SwiftClass
; CHECK: _TtERR ---> <ERROR TYPE>
; CHECK: _TtGSqGSaC5sugar7MyClass__ ---> [sugar.MyClass]?
; CHECK: _TtGSaGSqC5sugar7MyClass__ ---> [sugar.MyClass?]
; CHECK: _TtGV12generic_args7WrapperQq_FS0_4initUS_9AProtocol__FMGS0_Q__US1___FT4fromGS0_Q___GS0_Qd____ ---> generic_args.Wrapper<(archetype 0 of generic_args.Wrapper.init <A : generic_args.AProtocol>(generic_args.Wrapper<A>.Type) -> <B : generic_args.AProtocol>(from : generic_args.Wrapper<B>) -> generic_args.Wrapper<A>)>
; CHECK: _TtaC9typealias5DWARF9DIEOffset ---> typealias.DWARF.DIEOffset
; CHECK: _TtaSs3Int ---> Swift.Int
; CHECK: _TTRXFo_dSc_dSb_XFo_iSc_iSb_ ---> reabstraction thunk helper from @callee_owned (@unowned Swift.UnicodeScalar) -> (@unowned Swift.Bool) to @callee_owned (@in Swift.UnicodeScalar) -> (@out Swift.Bool)
; CHECK: _TTRXFo_dSi_dGSqSi__XFo_iSi_iGSqSi__ ---> reabstraction thunk helper from @callee_owned (@unowned Swift.Int) -> (@unowned Swift.Int?) to @callee_owned (@in Swift.Int) -> (@out Swift.Int?)
; CHECK: _TTRG0_R_XFo_iV18switch_abstraction1A_iq__XFo_dS0__iq__ ---> reabstraction thunk helper <T_0_0> from @callee_owned (@in switch_abstraction.A) -> (@out T_0_0) to @callee_owned (@unowned switch_abstraction.A) -> (@out T_0_0)
; CHECK: _TFCF5types1gFT1bSb_T_L0_10Collection3zimfS0_FT_T_ ---> types.(g (b : Swift.Bool) -> ()).(Collection #2).zim (types.(g (b : Swift.Bool) -> ()).(Collection #2))() -> ()
; CHECK: _TFF17capture_promotion22test_capture_promotionFT_FT_SiU_FT_Si_promote0 ---> capture_promotion.(test_capture_promotion () -> () -> Swift.Int).(closure #1) with unmangled suffix "_promote0"
; CHECK: _TFIVSs8_Processi10_argumentsGSaSS_U_FT_GSaSS_ ---> Swift._Process.(variable initialization expression)._arguments : [Swift.String] with unmangled suffix "U_FT_GSaSS_"
; CHECK: _TFIvVSs8_Process10_argumentsGSaSS_iU_FT_GSaSS_ ---> Swift._Process.(_arguments : [Swift.String]).(variable initialization expression).(closure #1)
; CHECK: _TFCSo1AE ---> ObjectiveC.A.__ivar_destroyer
; CHECK: _TFCSo1Ae ---> ObjectiveC.A.__ivar_initializer
; CHECK: _TTWC13call_protocol1CS_1PS_FS1_3fooU_fRQPS1_FT_Si ---> protocol witness for call_protocol.P.foo (inout call_protocol.P.Self)() -> Swift.Int in conformance call_protocol.C : call_protocol.P in call_protocol
; CHECK: _TFC12dynamic_self1X1ffDS0_FT_DS0_ ---> dynamic_self.X.f (Self)() -> Self
; CHECK: _TTSg5Si___TFSqCU__fMGSqQ__FT_GSqQ__ ---> generic specialization <Swift.Int> of Swift.Optional.init <A>(A?.Type)() -> A?
; CHECK: _TTSg5SiSiSs3FooSs_Sf___TFSqCU__fMGSqQ__FT_GSqQ__ ---> generic specialization <Swift.Int with Swift.Int : Swift.Foo in Swift, Swift.Float> of Swift.Optional.init <A>(A?.Type)() -> A?
; CHECK: _TTSg5Si_Sf___TFSqCU__fMGSqQ__FT_GSqQ__ ---> generic specialization <Swift.Int, Swift.Float> of Swift.Optional.init <A>(A?.Type)() -> A?
; CHECK: _TTSg5Si_Sf___TFSqCU__fMGSqQ__FT_GSqQ__ ---> generic specialization <Swift.Int, Swift.Float> of Swift.Optional.init <A>(A?.Type)() -> A?
; CHECK: _TTSgS ---> _TTSgS
; CHECK: _TTSg5S ---> _TTSg5S
; CHECK: _TTSgSi ---> _TTSgSi
; CHECK: _TTSg5Si ---> _TTSg5Si
; CHECK: _TTSgSi_ ---> _TTSgSi_
; CHECK: _TTSgSi__ ---> _TTSgSi__
; CHECK: _TTSgSiS_ ---> _TTSgSiS_
; CHECK: _TTSgSi__xyz ---> _TTSgSi__xyz
; CHECK: _TTSg5Si___TTSg5Si___TFSqCU__fMGSqQ__FT_GSqQ__ ---> generic specialization <Swift.Int> of generic specialization <Swift.Int> of Swift.Optional.init <A>(A?.Type)() -> A?
; CHECK: _TTSg5VSs5UInt8___TFV10specialize3XXXCU__fMGS0_Q__FT1tQ__GS0_Q__ ---> generic specialization <Swift.UInt8> of specialize.XXX.init <A>(specialize.XXX<A>.Type)(t : A) -> specialize.XXX<A>
; CHECK: _TPA__TTRXFo_oSSoSS_dSb_XFo_iSSiSS_dSb_31 ---> partial apply forwarder for reabstraction thunk helper from @callee_owned (@owned Swift.String, @owned Swift.String) -> (@unowned Swift.Bool) to @callee_owned (@in Swift.String, @in Swift.String) -> (@unowned Swift.Bool) with unmangled suffix "31"
; CHECK: _TsC4Meow5MyCls9subscriptFT1iSi_Sf ---> Meow.MyCls.subscript (i : Swift.Int) -> Swift.Float
; CHECK: _TF8manglingX22egbpdajGbuEbxfgehfvwxnFT_T_ ---> mangling.ليهمابتكلموشعربي؟ () -> ()
; CHECK: _TF8manglingX24ihqwcrbEcvIaIdqgAFGpqjyeFT_T_ ---> mangling.他们为什么不说中文 () -> ()
; CHECK: _TF8manglingX27ihqwctvzcJBfGFJdrssDxIboAybFT_T_ ---> mangling.他們爲什麽不說中文 () -> ()
; CHECK: _TF8manglingX30Proprostnemluvesky_uybCEdmaEBaFT_T_ ---> mangling.Pročprostěnemluvíčesky () -> ()
; CHECK: _TF8manglingXoi7p_qcaDcFTSiSi_Si ---> mangling.«+» infix (Swift.Int, Swift.Int) -> Swift.Int
; CHECK: _TF8manglingoi2qqFTSiSi_T_ ---> mangling.?? infix (Swift.Int, Swift.Int) -> ()
; CHECK: _TFE11ext_structAV11def_structA1A4testfRS1_FT_T_ ---> ext.ext_structA.def_structA.A.test (inout def_structA.A)() -> ()
; CHECK: _TF13devirt_accessP5_DISC15getPrivateClassFT_CS_P5_DISC12PrivateClass ---> devirt_access.(getPrivateClass in _DISC) () -> devirt_access.(PrivateClass in _DISC)
; CHECK: _TF4mainP5_mainX3wxaFT_T_ ---> main.(λ in _main) () -> ()
; CHECK: _TtPMP_ ---> protocol<>.Type
; CHECK: _TFCSs13_NSSwiftArray29canStoreElementsOfDynamicTypefS_FPMP_Sb ---> Swift._NSSwiftArray.canStoreElementsOfDynamicType (Swift._NSSwiftArray)(protocol<>.Type) -> Swift.Bool
; CHECK: _TFCSs13_NSSwiftArrayg17staticElementTypePMP_ ---> Swift._NSSwiftArray.staticElementType.getter : protocol<>.Type
; CHECK: _TFCSs17_DictionaryMirrorg9valueTypePMP_ ---> Swift._DictionaryMirror.valueType.getter : protocol<>.Type
; CHECK: _TPA__TFFVSs11GeneratorOfcU__FMGS_Q__USs13GeneratorType__FQ_GS_Qd___U_FT_GSqQd___ ---> partial apply forwarder for Swift.GeneratorOf.(init <A>(Swift.GeneratorOf<A>.Type) -> <B : Swift.GeneratorType>(B) -> Swift.GeneratorOf<A>).(closure #1)
; CHECK: _TPA__TFFVSs10SequenceOfcU__FMGS_Q__USs13GeneratorType__FFT_Q_GS_Qd___U_FT_GVSs11GeneratorOfQd___ ---> partial apply forwarder for Swift.SequenceOf.(init <A>(Swift.SequenceOf<A>.Type) -> <B : Swift.GeneratorType>(() -> B) -> Swift.SequenceOf<A>).(closure #1)
; CHECK: _TPA__TFFVSs6SinkOfcU__FMGS_Q__USs8SinkType__FQ_GS_Qd___U_FQd__T_ ---> partial apply forwarder for Swift.SinkOf.(init <A>(Swift.SinkOf<A>.Type) -> <B : Swift.SinkType>(B) -> Swift.SinkOf<A>).(closure #1)
; CHECK: _TTSf1cl35_TFF7specgen6callerFSiT_U_FTSiSi_T_Si___TF7specgen12take_closureFFTSiSi_T_T_ ---> function signature specialization <Arg[0] = Closure Propagated> of specgen.take_closure ((Swift.Int, Swift.Int) -> ()) -> ()
; CHECK: _TTSf1cl35_TFF7specgen6callerFSiT_U_FTSiSi_T_Si___TTSg5Si___TF7specgen12take_closureFFTSiSi_T_T_ ---> function signature specialization <Arg[0] = Closure Propagated> of generic specialization <Swift.Int> of specgen.take_closure ((Swift.Int, Swift.Int) -> ()) -> ()
; CHECK: _TTSg5Si___TTSf1cl35_TFF7specgen6callerFSiT_U_FTSiSi_T_Si___TF7specgen12take_closureFFTSiSi_T_T_ ---> generic specialization <Swift.Int> of function signature specialization <Arg[0] = Closure Propagated> of specgen.take_closure ((Swift.Int, Swift.Int) -> ()) -> ()
; CHECK: _TTSf1cpfr24_TF8capturep6helperFSiT__n___TTRXFo_dSi_dT__XFo_iSi_dT__ ---> function signature specialization <Arg[0] = Constant Propagated> of reabstraction thunk helper from @callee_owned (@unowned Swift.Int) -> (@unowned ()) to @callee_owned (@in Swift.Int) -> (@unowned ())
; CHECK: _TTSf1cpi0_cpfl0_cpse0v4u123_cpg53globalinit_33_06E7F1D906492AE070936A9B58CBAE1C_token8_cpfr36_TFtest_capture_propagation2_closure___TF7specgen12take_closureFFTSiSi_T_T_ ---> function signature specialization <Arg[0] = Constant Propagated, Arg[1] = Constant Propagated, Arg[2] = Constant Propagated, Arg[3] = Constant Propagated, Arg[4] = Constant Propagated> of specgen.take_closure ((Swift.Int, Swift.Int) -> ()) -> ()
; CHECK: _TTSf0s_s_d___TTSg5VSs16_REPLExitHandler___TFVSs22_ContiguousArrayBufferCU__fMGS_Q__FT5countSi15minimumCapacitySi_GS_Q__ ---> function signature specialization <Arg[0] = Exploded, Arg[1] = Exploded, Arg[2] = Dead> of generic specialization <Swift._REPLExitHandler> of Swift._ContiguousArrayBuffer.init <A>(Swift._ContiguousArrayBuffer<A>.Type)(count : Swift.Int, minimumCapacity : Swift.Int) -> Swift._ContiguousArrayBuffer<A>
; CHECK: _TTSf0gs___TFVSs11_StringCore15_invariantCheckfS_FT_T_ ---> function signature specialization <Arg[0] = Owned To Guaranteed and Exploded> of Swift._StringCore._invariantCheck (Swift._StringCore)() -> ()
; CHECK: _TTSf1n_n_n_n___TTSf2s_n_n___TTSg5GVSs15EmptyCollectionVSs6UInt16_GS_S0__Ss14CollectionTypeSs_GVSs14EmptyGeneratorS0__GS2_S0__Ss13GeneratorTypeSs_SiSiSs16ForwardIndexTypeSs_SiSiSs18_SignedIntegerTypeSs_SiSiSs33_BuiltinIntegerLiteralConvertibleSs_Si_S0____TFVSs11_StringCore12replaceRangefRS_USs14CollectionType_USs13GeneratorType_Ss16ForwardIndexType_Ss18_SignedIntegerType_Ss33_BuiltinIntegerLiteralConvertible____FTGVSs5RangeSi_4withQ__T_ ---> function signature specialization <> of function signature specialization <Arg[0] = Exploded> of generic specialization <Swift.EmptyCollection<Swift.UInt16> with Swift.EmptyCollection<Swift.UInt16> : Swift.CollectionType in Swift, Swift.EmptyGenerator<Swift.UInt16> with Swift.EmptyGenerator<Swift.UInt16> : Swift.GeneratorType in Swift, Swift.Int with Swift.Int : Swift.ForwardIndexType in Swift, Swift.Int with Swift.Int : Swift._SignedIntegerType in Swift, Swift.Int with Swift.Int : Swift._BuiltinIntegerLiteralConvertible in Swift, Swift.Int, Swift.UInt16> of Swift._StringCore.replaceRange (inout Swift._StringCore)<A : Swift.CollectionType>(Swift.Range<Swift.Int>, with : A) -> ()
; CHECK: _TTSf2g___TTSf2s_d___TFVSs11_StringCoreCfMS_FVSs13_StringBufferS_ ---> function signature specialization <Arg[0] = Owned To Guaranteed> of function signature specialization <Arg[0] = Exploded, Arg[1] = Dead> of Swift._StringCore.init (Swift._StringCore.Type)(Swift._StringBuffer) -> Swift._StringCore
; CHECK: _TTSf3d_i_d_i_d_i___TFVSs11_StringCoreCfMS_FVSs13_StringBufferS_ ---> function signature specialization <Arg[0] = Dead, Arg[1] = Value Promoted from InOut, Arg[2] = Dead, Arg[3] = Value Promoted from InOut, Arg[4] = Dead, Arg[5] = Value Promoted from InOut> of Swift._StringCore.init (Swift._StringCore.Type)(Swift._StringBuffer) -> Swift._StringCore
; CHECK: _TTSg5XPMTP4main9AnyObject____TFVSs22_ContiguousArrayBufferCU__fMGS_Q__FT5countSi15minimumCapacitySi_GS_Q__ ---> generic specialization <@thick main.AnyObject.Type> of Swift._ContiguousArrayBuffer.init <A>(Swift._ContiguousArrayBuffer<A>.Type)(count : Swift.Int, minimumCapacity : Swift.Int) -> Swift._ContiguousArrayBuffer<A>
; CHECK: _TTSg5XPMoP4main9AnyObject____TFVSs22_ContiguousArrayBufferCU__fMGS_Q__FT5countSi15minimumCapacitySi_GS_Q__ ---> generic specialization <@objc main.AnyObject.Type> of Swift._ContiguousArrayBuffer.init <A>(Swift._ContiguousArrayBuffer<A>.Type)(count : Swift.Int, minimumCapacity : Swift.Int) -> Swift._ContiguousArrayBuffer<A>
; CHECK: _TTSg5XPMtP4main9AnyObject____TFVSs22_ContiguousArrayBufferCU__fMGS_Q__FT5countSi15minimumCapacitySi_GS_Q__ ---> generic specialization <@thin main.AnyObject.Type> of Swift._ContiguousArrayBuffer.init <A>(Swift._ContiguousArrayBuffer<A>.Type)(count : Swift.Int, minimumCapacity : Swift.Int) -> Swift._ContiguousArrayBuffer<A>
; CHECK: _TTSg5XMTBi32____TFVSs22_ContiguousArrayBufferCU__fMGS_Q__FT5countSi15minimumCapacitySi_GS_Q__ ---> generic specialization <@thick Builtin.Int32.Type> of Swift._ContiguousArrayBuffer.init <A>(Swift._ContiguousArrayBuffer<A>.Type)(count : Swift.Int, minimumCapacity : Swift.Int) -> Swift._ContiguousArrayBuffer<A>
; CHECK: _TTSg5XMoBi32____TFVSs22_ContiguousArrayBufferCU__fMGS_Q__FT5countSi15minimumCapacitySi_GS_Q__ ---> generic specialization <@objc Builtin.Int32.Type> of Swift._ContiguousArrayBuffer.init <A>(Swift._ContiguousArrayBuffer<A>.Type)(count : Swift.Int, minimumCapacity : Swift.Int) -> Swift._ContiguousArrayBuffer<A>
; CHECK: _TTSg5XMtBi32____TFVSs22_ContiguousArrayBufferCU__fMGS_Q__FT5countSi15minimumCapacitySi_GS_Q__ ---> generic specialization <@thin Builtin.Int32.Type> of Swift._ContiguousArrayBuffer.init <A>(Swift._ContiguousArrayBuffer<A>.Type)(count : Swift.Int, minimumCapacity : Swift.Int) -> Swift._ContiguousArrayBuffer<A>
