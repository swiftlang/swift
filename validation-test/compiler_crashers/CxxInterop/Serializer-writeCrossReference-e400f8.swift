// {"kind":"emit-sil","signature":"swift::serialization::Serializer::writeCrossReference(swift::Decl const*)","signatureNext":"Serializer::writeASTBlockEntity"}
// RUN: not --crash %target-swift-frontend -emit-sil -cxx-interoperability-mode=default -I %S/Inputs/Serializer-writeCrossReference-4270e4 -emit-module-path /dev/null %s

import PrivateTypedef
func f(_ x: A.BB) {}
