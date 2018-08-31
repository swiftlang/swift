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
// RUN: %{python} %utils/process-stats-dir.py --evaluate-delta 'NumTotalClangImportedEntities < -10' %t/stats-pre %t/stats-post

import NamedLazyMembers

public func foo(d: SimpleDoer) {
  let _ = d.categoricallyDoSomeWork()
  let _ = d.categoricallyDoSomeWork(withSpeed:10)
  let _ = d.categoricallyDoVeryImportantWork(speed:10, thoroughness:12)
  let _ = d.categoricallyDoSomeWorkWithSpeed(speed:10, levelOfAlacrity:12)
}
