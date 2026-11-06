// {"kind":"emit-sil","signature":"swift::ModuleFile::getType(llvm::PointerEmbeddedInt<unsigned int, 31>)","signatureNext":"SILDeserializer::readSILInstruction"}
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -O -cross-module-optimization -cxx-interoperability-mode=default -I %S/Inputs/ModuleFile-getType-9aa080 %S/Inputs/ModuleFile-getType-9aa080/Lib.swift -module-name Lib -o %t/Lib.swiftmodule
// RUN: not --crash %target-swift-frontend -emit-sil -O -cxx-interoperability-mode=default -I %S/Inputs/ModuleFile-getType-9aa080 -I %t %s

import Lib

public func baz() -> Bool { bar() }
