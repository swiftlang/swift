// RUN: %target-swift-frontend -enable-experimental-feature AddressableTypes -enable-experimental-feature Lifetimes -enable-library-evolution -emit-sil -verify %s

// REQUIRES: swift_feature_AddressableTypes
// REQUIRES: swift_feature_Lifetimes

internal struct Wrapper {
    let inner: Resilient

    @_lifetime(borrow self)
    borrowing func getSpan() -> RawSpan { self.inner.getSpan() }
}

public struct Resilient {
    var field: AnyObject

    @_lifetime(borrow self)
    borrowing func getSpan() -> RawSpan { fatalError() }
}

/*
// TODO (rdar://151268401): We still get spurious errors about escaping `self`
// in cases where the wrapped type is concretely addressable-for-dependencies.
internal struct AFDWrapper {
    let inner: AFDResilient

    @_lifetime(borrow self)
    borrowing func getSpan() -> RawSpan { self.inner.getSpan() }
}

@_addressableForDependencies
public struct AFDResilient {
    @_lifetime(borrow self)
    borrowing func getSpan() -> RawSpan { fatalError() }
}
*/
