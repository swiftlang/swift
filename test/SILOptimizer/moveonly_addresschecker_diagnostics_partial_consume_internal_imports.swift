// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend                                      \
// RUN:     %t/Library.swift                                        \
// RUN:     -emit-module                                            \
// RUN:     -module-name Library                                    \
// RUN:     -emit-module-path %t/Library.swiftmodule

// RUN: %target-swift-frontend                                      \
// RUN:     %t/Downstream.swift                                     \
// RUN:     -emit-sil -verify                                       \
// RUN:     -sil-verify-all                                         \
// RUN:     -enable-upcoming-feature InternalImportsByDefault       \
// RUN:     -I %t

// REQUIRES: swift_feature_InternalImportsByDefault

//--- Library.swift

public struct Inner: ~Copyable {}

public struct Outer: ~Copyable {
  public var inner: Inner
}

@frozen
public struct OuterFrozen: ~Copyable {
  public var inner: Inner
}

//--- Downstream.swift

import Library

func consume_outer(_ o: consuming Outer) -> Inner {
  o.inner // expected-error{{cannot partially consume 'o' of non-frozen type 'Outer' imported from 'Library'}}
}

func consume_outer_frozen(_ o: consuming OuterFrozen) -> Inner {
  o.inner
}
