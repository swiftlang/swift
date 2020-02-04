// REQUIRES: objc_interop
// REQUIRES: OS=macosx
// RUN: rm -rf %t && mkdir -p %t/stats-pre && mkdir -p %t/stats-post
//
// Prime module cache
// RUN: %target-swift-frontend -typecheck -I %S/Inputs/NamedLazyMembers -typecheck -primary-file %s %S/Inputs/NamedLazyMembers/NamedLazyMembersExt.swift
//
// Check that named-lazy-member-loading reduces the number of Decls deserialized
// RUN: %target-swift-frontend -typecheck -I %S/Inputs/NamedLazyMembers -disable-named-lazy-member-loading -stats-output-dir %t/stats-pre -primary-file %s %S/Inputs/NamedLazyMembers/NamedLazyMembersExt.swift
// RUN: %target-swift-frontend -typecheck -I %S/Inputs/NamedLazyMembers -stats-output-dir %t/stats-post -primary-file %s %S/Inputs/NamedLazyMembers/NamedLazyMembersExt.swift
// RUN: %{python} %utils/process-stats-dir.py --evaluate-delta 'NumTotalClangImportedEntities <= -10' %t/stats-pre %t/stats-post

import NamedLazyMembers

public func bar() {
  let d = SimpleDoerSubclass(value: 123)!
  let _ = d.simplyDoVeryImportantWork(speed: 10, motivation: 42)
}

public func foo() {
  let d = SimpleDoer(value: 123)!
  let _ = d.simplyDoSomeWork()
  let _ = d.simplyDoSomeWork(withSpeed:10)
  let _ = d.simplyDoVeryImportantWork(speed:10, thoroughness:12)
  let _ = d.simplyDoSomeWorkWithSpeed(speed:10, levelOfAlacrity:12)
}

// Make sure that simply subclassing an imported subclass doesn't page in all
// members.
class MostDoerSubclass : SimpleDoerSubclass {}
