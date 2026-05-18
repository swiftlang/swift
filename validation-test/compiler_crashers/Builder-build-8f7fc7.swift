// {"kind":"typecheck","original":"3d5176b0","signature":"swift::ActorReferenceResult::Builder::build()","signatureNext":"ActorReferenceResult::forReference"}
// RUN: not --crash %target-swift-frontend -typecheck %s
actor a
  struct d
    extension Actor where Self: a {
        b<c>(d  -> c)
      let e = a().b { f in
