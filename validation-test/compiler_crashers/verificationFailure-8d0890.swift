// {"kind":"emit-silgen","original":"be1cdd08","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
// REQUIRES: OS=macosx
import Foundation
var a = 1
[a] as [NSNumber? ]
