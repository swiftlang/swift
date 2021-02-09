// REQUIRES: objc_interop
// REQUIRES: OS=macosx
// RUN: rm -rf %t && mkdir -p %t/stats-pre && mkdir -p %t/stats-post
//
// Prime module cache
// RUN: %target-swift-frontend -typecheck -I %S/Inputs/NamedLazyMembers -typecheck %s -swift-version 5
//
// Check that named-lazy-member-loading reduces the number of Decls deserialized
// RUN: %target-swift-frontend -typecheck -I %S/Inputs/NamedLazyMembers -disable-named-lazy-member-loading -stats-output-dir %t/stats-pre %s -swift-version 5
// RUN: %target-swift-frontend -typecheck -I %S/Inputs/NamedLazyMembers -stats-output-dir %t/stats-post %s -swift-version 5
// RUN: %{python} %utils/process-stats-dir.py --evaluate-delta 'NumTotalClangImportedEntities < -10' %t/stats-pre %t/stats-post

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

extension PrivateDoer {
  public var count: Int { return 0 }
  public func object(forKey: NSObject?) {}
}

public func foo(d: PrivateDoer) {
  _ = d.count
  _ = d.object

  let _ = d.__count
  let _ = d.__object(forKey: nil)
}