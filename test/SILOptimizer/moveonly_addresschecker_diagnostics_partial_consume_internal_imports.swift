// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend                                      \
// RUN:     %t/Library.swift                                        \
// RUN:     -emit-module                                            \
// RUN:     -module-name Library                                    \
// RUN:     -emit-module-path %t/Library.swiftmodule

// Regression: an `internal import` of another module gives that
// module's public types `Internal` access scope from the client's
// perspective. The MoveOnlyChecker's partial-consumption legality
// analysis previously asserted that the type's formal access scope was
// public-or-package at this point, which no longer holds. The
// assertion has been relaxed; the downstream frozen-vs-nonfrozen check
// is unchanged.

// RUN: %target-swift-frontend                                      \
// RUN:     %t/Downstream.swift                                     \
// RUN:     -emit-sil -verify                                       \
// RUN:     -sil-verify-all                                         \
// RUN:     -I %t

//--- Library.swift

public struct Source: ~Copyable {
    public init() {}
}

@frozen public struct FrozenWrapper: ~Copyable {
    public var source: Source
    public init(source: consuming Source) { self.source = source }
}

public struct NonFrozenWrapper: ~Copyable {
    public var source: Source
    public init(source: consuming Source) { self.source = source }
}

//--- Downstream.swift

internal import Library

// @frozen: partial consume across module boundaries is allowed.
func frozenPartialConsume() {
    let w = FrozenWrapper(source: Source())
    _ = consume w.source
}

// Non-@frozen: partial consume is rejected with the imported-nonfrozen
// diagnostic.
func nonfrozenPartialConsume() {
    let w = NonFrozenWrapper(source: Source())
    _ = consume w.source // expected-error {{cannot partially consume 'w' of non-frozen type 'NonFrozenWrapper' imported from 'Library'}}
}
