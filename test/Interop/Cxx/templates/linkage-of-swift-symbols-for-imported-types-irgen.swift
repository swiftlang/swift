// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import LinkageOfSwiftSymbolsForImportedTypes

public func forceValueWitnessTableCreation() -> Any {
  let magicNumber = MagicNumber()
  return WrappedMagicNumber(t: magicNumber)
}

public func getMagicNumberForLinkageComparison() -> Any {
	return MagicNumber()
}

// CHECK: $sSo11MagicNumberVWV" = linkonce_odr hidden constant
// CHECK: $sSo11MagicNumberVMn" = linkonce_odr hidden constant
// CHECK: $sSo11MagicNumberVMf" = linkonce_odr hidden constant
// CHECK: $sSo11MagicNumberVML" = linkonce_odr hidden global

// CHECK: $sSo0031MagicWrapperMagicNumber_IJFJhAbVWV" = linkonce_odr hidden constant
// CHECK: $sSo0031MagicWrapperMagicNumber_IJFJhAbVMn" = linkonce_odr hidden constant
// CHECK: $sSo0031MagicWrapperMagicNumber_IJFJhAbVMf" = linkonce_odr hidden constant
// CHECK: $sSo0031MagicWrapperMagicNumber_IJFJhAbVML" = linkonce_odr hidden global

// CHECK: $sSo11MagicNumberVMB" = linkonce_odr hidden constant
// CHECK: $sSo11MagicNumberVMF" = linkonce_odr hidden constant

// CHECK: $sSo0031MagicWrapperMagicNumber_IJFJhAbVMB" = linkonce_odr hidden constant
// CHECK: $sSo0031MagicWrapperMagicNumber_IJFJhAbVMF" = linkonce_odr hidden constant
