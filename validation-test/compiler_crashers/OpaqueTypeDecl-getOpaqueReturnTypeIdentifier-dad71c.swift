// {"kind":"typecheck","original":"2e672fab","signature":"swift::OpaqueTypeDecl::getOpaqueReturnTypeIdentifier() const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b {
  protocol c {
    struct d {
      protocol e
        struct f<g
          func h -> f<some e>
