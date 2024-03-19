// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserASTGen -enable-experimental-feature ValidateTypeReprASTGen -enable-experimental-feature NoncopyableGenerics

// -enable-experimental-feature requires an asserts build
// REQUIRES: asserts
// REQUIRES: noncopyable_generics

let _: any ~Copyable
