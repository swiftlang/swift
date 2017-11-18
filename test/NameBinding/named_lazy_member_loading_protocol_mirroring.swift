// REQUIRES: objc_interop
// REQUIRES: OS=macosx
// RUN: rm -rf %t && mkdir -p %t/stats-pre && mkdir -p %t/stats-post
//
// Prime module cache
// RUN: %target-swift-frontend -typecheck -I %S/Inputs/NamedLazyMembers -typecheck %s
//
// Check that named-lazy-member-loading reduces the number of Decls deserialized
// RUN: %target-swift-frontend -typecheck -I %S/Inputs/NamedLazyMembers -disable-named-lazy-member-loading -stats-output-dir %t/stats-pre %s
// RUN: %target-swift-frontend -typecheck -I %S/Inputs/NamedLazyMembers -stats-output-dir %t/stats-post %s

import NamedLazyMembers

public func foo(d: MirroringDoer) {
  let _ = MirroringDoer.mirroredBaseClassMethod()
  let _ = MirroringDoer.mirroredDerivedClassMethod()
  let _ = d.mirroredBaseInstanceMethod()
  let _ = d.mirroredDerivedInstanceMethod()
}

public func foo(d: DerivedFromMirroringDoer) {
  let _ = MirroringDoer.mirroredBaseClassMethod()
  let _ = MirroringDoer.mirroredDerivedClassMethod()
  let _ = d.mirroredBaseInstanceMethod()
  let _ = d.mirroredDerivedInstanceMethod()
}
