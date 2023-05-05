// RUN: %target-swift-emit-sil -sil-verify-all -verify -enable-library-evolution %s

// This test is used to validate that we properly handle library evolution code
// until we can get all of the normal moveonly_addresschecker_diagnostics test
// case to pass.

////////////////////////
// MARK: Deinit Tests //
////////////////////////

@_moveOnly public struct EmptyStruct {}
@_moveOnly public struct NonEmptyStruct {
    var e = EmptyStruct()
}
public class CopyableKlass {
    var s = NonEmptyStruct()
}

public func borrowVal(_ x: borrowing NonEmptyStruct) {}
public func borrowVal(_ x: borrowing EmptyStruct) {}

/////////////////
// MARK: Tests //
/////////////////

public struct DeinitTest : ~Copyable {
    deinit {}
}

public protocol P {}

// Once rdar://109170600 is fixed (which tracks us being unable to do this), we
// should be able to use the : ~Copyable syntax.
@_moveOnly
public struct GenericDeinitTest<T : P> {
    deinit {}
}

