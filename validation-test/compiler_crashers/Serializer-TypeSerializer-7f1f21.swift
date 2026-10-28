// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"9efe82e1","signature":"swift::TypeVisitor<swift::serialization::Serializer::TypeSerializer, void>::visit(swift::Type)","signatureNext":"Serializer::writeASTBlockEntity"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
let a = Swift
