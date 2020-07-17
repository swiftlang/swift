// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// RUN: %target-swift-frontend -Xcc -target -Xcc x86_64-unknown-windows-msvc -Xcc -fno-PIC -emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// RUN: %target-swift-frontend -Xcc -target -Xcc x86_64-apple-macosx10.9 -Xcc -fno-PIC -emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// RUN: %target-swift-frontend -Xcc -target -Xcc arm64-apple-ios11.2.0 -Xcc -fno-PIC -emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import CanonicalTypes

public func testCanonicalTypes() -> Bool {
  // multiple typedeffed types with the same canonical type are the same type
  // from the typechecking perspective.
  let magicNumber = MagicNumber()
  var wrappedMagicNumberA = WrappedMagicNumberA(t: magicNumber)

  var wrappedMagicNumberB: WrappedMagicNumberA =
    WrappedMagicNumberB(t: magicNumber)
  return wrappedMagicNumberA.callGetInt() == wrappedMagicNumberB.callGetInt()
}

// CHECK: // testCanonicalTypes()
// CHECK: sil @$s4main18testCanonicalTypesSbyF : $@convention(thin) () -> Bool
// CHECK: [[A:%.*]] = alloc_stack $__CxxTemplateInst12MagicWrapperI11MagicNumberE, var, name "wrappedMagicNumberA"
// CHECK: [[B:%.*]] = alloc_stack $__CxxTemplateInst12MagicWrapperI11MagicNumberE, var, name "wrappedMagicNumberB"
// CHECK: [[A_ACCESS:%.*]] = begin_access [modify] [static] [[A]] : $*__CxxTemplateInst12MagicWrapperI11MagicNumberE
// CHECK: // function_ref {{_ZNK12MagicWrapperI11MagicNumberE10callGetIntEv|\?callGetInt@\?\$MagicWrapper@UMagicNumber@@@@QEBAHXZ}}
// CHECK: [[A_CALL_GET_INT:%.*]] = function_ref @{{_ZNK12MagicWrapperI11MagicNumberE10callGetIntEv|\?callGetInt@\?\$MagicWrapper@UMagicNumber@@@@QEBAHXZ}} : $@convention(c) (@inout __CxxTemplateInst12MagicWrapperI11MagicNumberE) -> Int32
// CHECK: apply [[A_CALL_GET_INT]]([[A_ACCESS]]) : $@convention(c) (@inout __CxxTemplateInst12MagicWrapperI11MagicNumberE) -> Int32
// CHECK: [[B_ACCESS:%.*]] = begin_access [modify] [static] [[B]] : $*__CxxTemplateInst12MagicWrapperI11MagicNumberE
// CHECK: // function_ref {{_ZNK12MagicWrapperI11MagicNumberE10callGetIntEv|\?callGetInt@\?\$MagicWrapper@UMagicNumber@@@@QEBAHXZ}}
// CHECK: [[B_CALL_GET_INT:%.*]] = function_ref @{{_ZNK12MagicWrapperI11MagicNumberE10callGetIntEv|\?callGetInt@\?\$MagicWrapper@UMagicNumber@@@@QEBAHXZ}} : $@convention(c) (@inout __CxxTemplateInst12MagicWrapperI11MagicNumberE) -> Int32
// CHECK: apply [[B_CALL_GET_INT]]([[B_ACCESS]]) : $@convention(c) (@inout __CxxTemplateInst12MagicWrapperI11MagicNumberE) -> Int32
// CHECK: } // end sil function '$s4main18testCanonicalTypesSbyF'

// CHECK: // {{_ZNK12MagicWrapperI11MagicNumberE10callGetIntEv|\?callGetInt@\?\$MagicWrapper@UMagicNumber@@@@QEBAHXZ}}
// CHECK: // clang name: MagicWrapper<MagicNumber>::callGetInt
// CHECK: sil [clang __CxxTemplateInst12MagicWrapperI11MagicNumberE.callGetInt] @{{_ZNK12MagicWrapperI11MagicNumberE10callGetIntEv|\?callGetInt@\?\$MagicWrapper@UMagicNumber@@@@QEBAHXZ}} : $@convention(c) (@inout __CxxTemplateInst12MagicWrapperI11MagicNumberE) -> Int32


