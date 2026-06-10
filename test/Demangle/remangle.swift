; This is not really a Swift source file: -*- Text -*-

%t.input: "A ---> B" ==> "A"
RUN: sed -ne '/--->/s/ *--->.*$//p' < %S/Inputs/manglings.txt > %t.input

RUN: swift-demangle -test-remangle < %t.input > %t.output
RUN: diff %t.input %t.output

// CHECK: _TVsP33_7B40D7ED6632C2BEA2CA3BFFD57E34358Mystruct
RUN: swift-demangle -remangle-objc-rt '$ss8Mystruct33_7B40D7ED6632C2BEA2CA3BFFD57E3435LLV' | %FileCheck %s

// CHECK-OLD: _TPF1_1?FT1Ps16UnsafeRawPointer_SV1r
RUN: swift-demangle -remangle-objc-rt '$s1_1?1PSVSVF1rP' | %FileCheck -check-prefix CHECK-OLD %s

// CHECK-OLD2: _TPSiP2$BMSPs16IteratorProtocol_
RUN: swift-demangle -remangle-objc-rt '$sSiStMSLB_p' | %FileCheck -check-prefix CHECK-OLD2 %s

// CHECK-OLD3: _TPsP2$HPA
RUN: swift-demangle -remangle-objc-rt '$ssTALHP' | %FileCheck -check-prefix CHECK-OLD3 %s

// CHECK-OLD4: _TPiVs14DefaultIndicesx6foobar
RUN: swift-demangle -remangle-objc-rt '$sSIxip6foobarP' | %FileCheck -check-prefix CHECK-OLD4 %s

// CHECK-GENERICEXT: _TtGCs23_ContiguousArrayStorageGVEsVs15FlattenSequence5IndexGV24StdlibCollectionUnittest30MinimalBidirectionalCollectionGS3_Si_____
RUN: swift-demangle -remangle-objc-rt '$ss23_ContiguousArrayStorageCys15FlattenSequenceVsE5IndexVy24StdlibCollectionUnittest020MinimalBidirectionalH0VyAIySiGG_GGD' | %FileCheck -check-prefix CHECK-GENERICEXT %s


// CHECK-OPAQUE: _TCFF1A5outerFT_T_U_FT_Qo_QOFES_PS_1P8decorateFT_QuVS_4Impl_L_9MenuState
RUN: swift-demangle -remangle-objc-rt '$s1A5outeryyFAA1PPAAE8decorateQryFQOyAA4ImplV_Qo_yXEfU_9MenuStateL_C' | %FileCheck -check-prefix CHECK-OPAQUE %s

// CHECK-OPAQUE-RT: MenuState #1 in closure #1 () -> <<opaque return type of (extension in A):A.P.decorate() -> some>>.0 in A.outer() -> (){{$}}
RUN: swift-demangle '_TCFF1A5outerFT_T_U_FT_Qo_QOFES_PS_1P8decorateFT_QuVS_4Impl_L_9MenuState' | %FileCheck -check-prefix CHECK-OPAQUE-RT %s
