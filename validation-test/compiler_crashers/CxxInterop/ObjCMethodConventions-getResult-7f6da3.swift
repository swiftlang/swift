// {"frontendArgs":["-emit-ir","-target","%target-cpu-apple-macosx13.0","-cxx-interoperability-mode=default"],"kind":"custom","signature":"(anonymous namespace)::ObjCMethodConventions::getResult(swift::Lowering::TypeLowering const&) const","signatureAssert":"Assertion failed: (resultType->isObjCRetainableType() || isCFTypedef(tl, resultType)), function getResult","signatureNext":"DestructureResults::destructure"}
// RUN: not --crash %target-swift-frontend -emit-ir -target %target-cpu-apple-macosx13.0 -cxx-interoperability-mode=default -I %S/Inputs/ObjCMethodConventions-getResult-7f6da3 %s
// REQUIRES: OS=macosx

import ArcStruct

let p: Any = ArcStructProvider.self
