// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Compile the middle Swift library LibWithXRef.
// RUN: %target-swift-frontend -emit-module %t/LibWithXRef.swift -I %t \
// RUN:   -module-name LibWithXRef -o %t/LibWithXRef.swiftmodule \
// RUN:   -swift-version 5

/// Build client fine in the expected behavior.
// RUN: %target-swift-frontend -c -O %t/Client.swift -I %t \
// RUN:   -validate-tbd-against-ir=none -swift-version 5

// Replace headers, changing the type of `foo`.
// RUN: cp %t/A_Changed.h %t/A.h
// RUN: not --crash %target-swift-frontend -c -O %t/Client.swift -I %t \
// RUN:   -validate-tbd-against-ir=none -swift-version 5 \
// RUN:   -diagnostic-style llvm 2> %t/out
// RUN: %FileCheck %t/A_Changed.h --input-file %t/out

// Replace headers, disappearing `foo`.
// RUN: cp %t/A_Disappeared.h %t/A.h
// RUN: not --crash %target-swift-frontend -c -O %t/Client.swift -I %t \
// RUN:   -validate-tbd-against-ir=none -swift-version 5 \
// RUN:   -diagnostic-style llvm 2> %t/out
// RUN: %FileCheck %t/A_Disappeared.h --input-file %t/out

//--- module.modulemap
module A {
    header "A.h"
}

//--- A.h
struct S {};
struct S *create() __attribute__((swift_name("S.init()")));
double foo(struct S *data) __attribute__((swift_name("S.foo(self:)")));

//--- A_Disappeared.h
struct S {};
struct S *create() __attribute__((swift_name("S.init()")));

// CHECK: error: reference to declaration 'S.foo' broken by a context change; 'S.foo' is not found, it was expected to be in 'A'
// CHECK: note: the declaration was expected to be found in module 'A' at '{{.*}}module.modulemap'
// CHECK-NOT: actually found

//--- A_Changed.h
struct S {};
struct S *create() __attribute__((swift_name("S.init()")));
float foo(struct S *data) __attribute__((swift_name("S.foo(self:)")));

// CHECK: error: reference to declaration 'S.foo' broken by a context change; the declaration kind of 'S.foo' from 'A' changed since building 'LibWithXRef'
// CHECK: note: the declaration was expected to be found in module 'A' at '{{.*}}module.modulemap'
// CHECK: note: a candidate was filtered out because of a type mismatch; expected: '(inout S) -> () -> Double', found: '(inout S) -> () -> Float'

//--- LibWithXRef.swift
import A

@inlinable
public func bar() {
    var s = S()
    s.foo()
}

//--- Client.swift
import LibWithXRef

bar()
