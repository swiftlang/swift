// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"0259d441","signature":"getCommentKind(llvm::StringRef)","signatureAssert":"Assertion failed: (Comment[0] == '/'), function getCommentKind","signatureNext":"RawCommentRequest::evaluate"}
// RUN: %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
struct a {
  var b: Int
}
