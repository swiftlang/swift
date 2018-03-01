// RUN: %target-swift-frontend -target x86_64-apple-macosx10.12 -assume-parsing-unqualified-ownership-sil -import-objc-header %S/Inputs/StaticInline.h %s -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -O -target x86_64-apple-macosx10.12 -assume-parsing-unqualified-ownership-sil -import-objc-header %S/Inputs/StaticInline.h %s -emit-ir | %FileCheck %s --check-prefix=OPT

// REQUIRES: objc_interop
// REQUIRES: CPU=x86_64
// REQUIRES: OS=macosx
import Foundation

@inline(never)
public func useClosure(_ dict: NSDictionary, _ action : (NSDictionary) -> ()) {
  action(dict)
}

@inline(never)
public func test(_ dict: NSDictionary) {
  useClosure(dict, { $0.objectEnumerator()} )
}

//  Don't tail call objc_retainAutoreleasedReturnValue as this would block the
//  autorelease return value optimization.

// callq  0x01ec08 ; symbol stub for: objc_msgSend
// movq   %rax, %rdi
// popq   %rbp  ;<== Blocks the handshake from objc_autoreleaseReturnValue
// jmp    0x01ec20 ; symbol stub for: objc_retainAutoreleasedReturnValue

// CHECK-LABEL: define {{.*}}swiftcc %TSo12NSEnumeratorC* @"$S34objc_retainAutoreleasedReturnValue4testyySo12NSDictionaryCFSo12NSEnumeratorCADcfU_"(%TSo12NSDictionaryC*)
// CHECK: entry:
// CHECK:   call {{.*}}@objc_msgSend
// CHECK:   notail call i8* @objc_retainAutoreleasedReturnValue
// CHECK:   ret %TSo12NSEnumeratorC*

// CHECK-LABEL: define {{.*}}swiftcc void @"$SSo12NSDictionaryCSo12NSEnumeratorCIgxo_ABIgx_TR"(%TSo12NSDictionaryC*, i8*, %swift.refcounted*)

// OPT-LABEL: define {{.*}}swiftcc void @"$S34objc_retainAutoreleasedReturnValue10useClosureyySo12NSDictionaryC_yADctF06$SSo12h43CSo12NSEnumeratorCIgxo_ABIgx_TR049$S34objc_bcD41Value4testyySo12a6CFSo12B7CADcfU_Tf3npf_nTf1nc_nTf4g_n"(%TSo12NSDictionaryC*)
// OPT: entry:
// OPT:   call {{.*}}@objc_msgSend
// OPT:   notail call i8* @objc_retainAutoreleasedReturnValue
// OPT:   ret void
