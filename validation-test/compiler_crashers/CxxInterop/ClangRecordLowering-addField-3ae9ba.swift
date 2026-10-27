// {"kind":"emit-ir","signature":"(anonymous namespace)::ClangRecordLowering::addField(swift::VarDecl*, swift::irgen::Size, swift::irgen::FixedTypeInfo const&, bool)","signatureAssert":"Assertion failed: (isZeroSized || offset >= NextOffset && \"adding fields out of order\"), function addField","signatureNext":"ClangRecordLowering::addOpaqueField"}
// RUN: not --crash %target-swift-frontend -emit-ir -cxx-interoperability-mode=default -I %S/Inputs/ClangRecordLowering-addField-3ae9ba %s
// RUN: not --crash %target-swift-frontend -emit-ir -cxx-interoperability-mode=default -I %S/Inputs/ClangRecordLowering-addField-3ae9ba %s -D DIAMOND

import Test
#if DIAMOND
let _ = D()
#else
let _ = C()
#endif
