// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Middle library WITHOUT clang function types: the serialized cross-reference
/// to S.fp records a function type with NO Clang type.
// RUN: %target-swift-frontend -emit-module %t/LibWithXRef.swift -I %t \
// RUN:   -module-name LibWithXRef -o %t/LibWithXRef.swiftmodule -swift-version 5

/// Client WITH clang function types: re-importing S.fp yields a function type
/// that DOES carry a Clang type. Expected (no cType) vs found (cType present)
/// differ only in Clang-type presence — the relaxed near-match must tolerate it
/// instead of aborting deserialization.
// RUN: %target-swift-frontend -c -O %t/Client.swift -I %t \
// RUN:   -use-clang-function-types -validate-tbd-against-ir=none -swift-version 5 2>&1 \
// RUN:   | %FileCheck %s --allow-empty

// CHECK-NOT: *** DESERIALIZATION FAILURE ***
// CHECK-NOT: broken by a context change

//--- module.modulemap
module A {
    header "A.h"
}

//--- A.h
struct S {
    int (*fp)(struct S *, unsigned long, void *);
};

//--- LibWithXRef.swift
import A

@inlinable
public func bar(_ s: inout S) {
    _ = s.fp
}

//--- Client.swift
import LibWithXRef
import A

public func drive(_ s: inout S) {
    bar(&s)
}
