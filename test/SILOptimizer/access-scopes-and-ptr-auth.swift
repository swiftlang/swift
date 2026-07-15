// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-build-swift -O -import-objc-header %t/c-header.h %t/test.swift -Xfrontend -sil-verify-all -emit-sil -o /dev/null

// REQUIRES: CPU=arm64e

//--- c-header.h

struct S {
    void (*__ptrauth(0, 1, 0xB966) fn)(void);
};

//--- test.swift

func target() {}

// Check that DeadAccessScopeElimination does not remove the `begin_access [signed]`.
// This would cause a SILVerifier crash.

public func test(_ out: UnsafeMutablePointer<S>) {
    let a = [S(fn: target)]
    out.pointee = a[0]
}

