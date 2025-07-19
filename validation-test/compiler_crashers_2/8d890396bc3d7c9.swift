// {"kind":"emit-silgen","original":"be1cdd08","signature":"swift::SILModule::print(llvm::raw_ostream&, swift::ModuleDecl*, swift::SILOptions const&, bool) const"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
// REQUIRES: OS=macosx
import Foundation
var a = 1
[a] as [NSNumber? ]
