
// RUN: %target-swift-frontend -module-name objc_retainAutoreleasedReturnValue -assume-parsing-unqualified-ownership-sil -import-objc-header %S/Inputs/StaticInline.h %s -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -module-name objc_retainAutoreleasedReturnValue -O -assume-parsing-unqualified-ownership-sil -import-objc-header %S/Inputs/StaticInline.h %s -emit-ir | %FileCheck %s --check-prefix=OPT

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

// OPT-LABEL: define {{.*}}swiftcc void @"$S34objc_retainAutoreleasedReturnValue4testyySo12NSDictionaryCFyADXEfU_"(%TSo12NSDictionaryC*)
// OPT: entry:
// OPT:   call {{.*}}@objc_msgSend
// OPT:   notail call i8* @objc_retainAutoreleasedReturnValue
// OPT:   ret void
