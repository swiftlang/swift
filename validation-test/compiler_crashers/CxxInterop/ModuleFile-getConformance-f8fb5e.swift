// {"kind":"emit-sil","signature":"swift::ModuleFile::getConformance(llvm::PointerEmbeddedInt<unsigned int, 31>)","signatureNext":"SILDeserializer::readWitnessTableEntries"}
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -cxx-interoperability-mode=default -I %S/Inputs/ModuleFile-getConformance-f8fb5e %S/Inputs/ModuleFile-getConformance-f8fb5e/Lib.swift -module-name Lib -o %t/Lib.swiftmodule
// RUN: not --crash %target-swift-frontend -emit-sil -O -cxx-interoperability-mode=default -I %S/Inputs/ModuleFile-getConformance-f8fb5e -I %t %s

import Lib
import ListProxy

public func test(_ p: ns.IntListProxy) -> [Int32] {
  return toArray(p)
}
