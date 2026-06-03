// {"extraArgs":["-experimental-allow-module-with-compiler-errors"],"kind":"emit-sil","original":"576e1339","signature":"swift::SILModule::print(swift::SILPrintContext&, swift::ModuleDecl*, bool) const","signatureNext":"writeSIL"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors %s
// REQUIRES: OS=macosx
import a
