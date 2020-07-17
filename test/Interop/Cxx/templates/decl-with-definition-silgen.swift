// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// RUN: %target-swift-frontend -Xcc -target -Xcc x86_64-unknown-windows-msvc -Xcc -fno-PIC -emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// RUN: %target-swift-frontend -Xcc -target -Xcc x86_64-apple-macosx10.9 -Xcc -fno-PIC -emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// RUN: %target-swift-frontend -Xcc -target -Xcc arm64-apple-ios11.2.0 -Xcc -fno-PIC -emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import DeclWithDefinition

public func getWrappedMagicNumber() -> CInt {
  let magicNumber = MagicNumber()
  var wrappedMagicNumber = PartiallyDefinedWrappedMagicNumber(t: magicNumber)
  return wrappedMagicNumber.callGetInt()
}

// CHECK: // getWrappedMagicNumber()
// CHECK: sil @$s4main21getWrappedMagicNumbers5Int32VyF : $@convention(thin) () -> Int32 {
// CHECK: [[MAGIC_NUMBER:%.*]] = struct $MagicNumber ()
// CHECK: [[_:%.*]] = struct $__CxxTemplateInst12MagicWrapperI11MagicNumberE ([[MAGIC_NUMBER]] : $MagicNumber)
// CHECK: // function_ref {{_ZNK12MagicWrapperI11MagicNumberE10callGetIntEv|\?callGetInt@\?\$MagicWrapper@UMagicNumber@@@@QEBAHXZ}}
// CHECK: [[_:%.*]] = function_ref @{{_ZNK12MagicWrapperI11MagicNumberE10callGetIntEv|\?callGetInt@\?\$MagicWrapper@UMagicNumber@@@@QEBAHXZ}} : $@convention(c) (@inout __CxxTemplateInst12MagicWrapperI11MagicNumberE) -> Int32

// CHECK: // {{_ZNK12MagicWrapperI11MagicNumberE10callGetIntEv|\?callGetInt@\?\$MagicWrapper@UMagicNumber@@@@QEBAHXZ}}
// CHECK: // clang name: MagicWrapper<MagicNumber>::callGetInt
// CHECK: sil [clang __CxxTemplateInst12MagicWrapperI11MagicNumberE.callGetInt] @{{_ZNK12MagicWrapperI11MagicNumberE10callGetIntEv|\?callGetInt@\?\$MagicWrapper@UMagicNumber@@@@QEBAHXZ}} : $@convention(c) (@inout __CxxTemplateInst12MagicWrapperI11MagicNumberE) -> Int32
