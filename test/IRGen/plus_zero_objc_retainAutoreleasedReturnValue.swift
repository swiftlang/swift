// REQUIRES: plus_zero_runtime

// RUN: %target-swift-frontend -module-name objc_retainAutoreleasedReturnValue -target x86_64-apple-macosx10.12 -assume-parsing-unqualified-ownership-sil -import-objc-header %S/Inputs/StaticInline.h %s -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -module-name objc_retainAutoreleasedReturnValue -O -target x86_64-apple-macosx10.12 -assume-parsing-unqualified-ownership-sil -import-objc-header %S/Inputs/StaticInline.h %s -emit-ir | %FileCheck %s --check-prefix=OPT

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

// CHECK-LABEL: define {{.*}}swiftcc void @"$S34objc_retainAutoreleasedReturnValue4testyySo12NSDictionaryCFyADXEfU_"(%TSo12NSDictionaryC*)
// CHECK: entry:
// CHECK:   call {{.*}}@objc_msgSend
// CHECK:   notail call i8* @objc_retainAutoreleasedReturnValue
// CHECK:   ret void

// CHECK-LABEL: define {{.*}}swiftcc void @"$SSo12NSDictionaryCSo12NSEnumeratorCIggo_ABIegg_TR"(%TSo12NSDictionaryC*, i8*, %swift.opaque*)

// OPT-LABEL: define {{.*}}swiftcc void @"$S34objc_retainAutoreleasedReturnValue10useClosureyySo12NSDictionaryC_yADXEtF06$SSo12h44CSo12NSEnumeratorCIggo_ABIegg_TR049$S34objc_bcD42Value4testyySo12a6CFSo12B8CADXEfU_Tf3npf_nTf1nc_n"(%TSo12NSDictionaryC*)
// OPT: entry:
// OPT:   call {{.*}}@objc_msgSend
// OPT:   notail call i8* @objc_retainAutoreleasedReturnValue
// OPT:   ret void
