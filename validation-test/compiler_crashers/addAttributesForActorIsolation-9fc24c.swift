// {"kind":"typecheck","original":"f7b7bce2","signature":"addAttributesForActorIsolation(swift::Decl*, swift::ActorIsolation)","signatureAssert":"Assertion failed: (!isa<ClassDecl>(decl) || !cast<ClassDecl>(decl)->isExplicitActor()), function addAttributesForActorIsolation","signatureNext":"computeActorIsolation"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@globalActor actor a {
  static  shared =
}
actor b: c
  @a class c
