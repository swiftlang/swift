// RUN: rm -rf %t && mkdir -p %t/stats-pre && mkdir -p %t/stats-post
//
// Compile swiftmodule with decl member name tables
// RUN: %target-swift-frontend -emit-module -o %t/NamedLazyMembers.swiftmodule -enable-named-lazy-member-loading %S/Inputs/NamedLazyMembers/NamedLazyMembers.swift
//
// Check that without lazy named member loading, we're importing too much.
// RUN: %target-swift-frontend -typecheck -I %t -typecheck -stats-output-dir %t/stats-pre %s
// RUN: %utils/process-stats-dir.py --evaluate 'NumDeclsDeserialized > 140' %t/stats-pre
//
// Check that with lazy named member loading, we're importing less.
// RUN: %target-swift-frontend -typecheck -I %t -enable-named-lazy-member-loading -stats-output-dir %t/stats-post %s
// RUN: %utils/process-stats-dir.py --evaluate 'NumDeclsDeserialized < 125' %t/stats-post

import NamedLazyMembers

public func foo(f: FooClass) {
  let _ = f.member1()
}
