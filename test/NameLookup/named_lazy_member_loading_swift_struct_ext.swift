// RUN: rm -rf %t && mkdir -p %t/stats-pre && mkdir -p %t/stats-post
//
// Compile swiftmodule with decl member name tables
// RUN: %target-swift-frontend -emit-module -o %t/NamedLazyMembers.swiftmodule %S/Inputs/NamedLazyMembers/NamedLazyMembers.swift
//
// Check that named-lazy-member-loading reduces the number of Decls deserialized
// RUN: %target-swift-frontend -typecheck -I %t -disable-named-lazy-member-loading -typecheck -stats-output-dir %t/stats-pre %s
// RUN: %target-swift-frontend -typecheck -I %t -stats-output-dir %t/stats-post %s
// RUN: %{python} %utils/process-stats-dir.py --evaluate-delta 'NumDeclsDeserialized < -5' %t/stats-pre %t/stats-post

import NamedLazyMembers

public func test(b: BaseExt) {
  let _ = b.memberFunc1()
}
