// {"kind":"typecheck","original":"307d2ff3","signature":"swift::ArgumentList::getSourceRange() const","signatureNext":"diagnoseArgumentLabelError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@dynamicMemberLookup protocol a {
  subscript(dynamicMember b: String)  Self
  struct c: a {
    d: {
      d
    }
    subscript( String)  c
