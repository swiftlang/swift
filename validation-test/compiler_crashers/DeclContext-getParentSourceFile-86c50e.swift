// {"kind":"typecheck","original":"c6a502fd","signature":"swift::DeclContext::getParentSourceFile() const","signatureNext":"IterableDeclContext::loadStorageMembers"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@freestanding(declaration) macro a()
@attached(extension) macro b() =
  #a
@b struct c {
}
