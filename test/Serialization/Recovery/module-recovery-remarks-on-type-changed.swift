// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Compile two libraries A and LibWithXRef.
// RUN: %target-swift-frontend -emit-module %t/LibWithXRef.swift -I %t \
// RUN:   -module-name LibWithXRef -o %t/LibWithXRef.swiftmodule \
// RUN:   -swift-version 5
// RUN: %target-swift-frontend -c -O %t/Client.swift -I %t \
// RUN:   -validate-tbd-against-ir=none -swift-version 5

// Replace headers, changing the type of `foo`.
// RUN: mv %t/A_DifferentAPI.h %t/A.h
// RUN: not --crash %target-swift-frontend -c -O %t/Client.swift -I %t \
// RUN:   -validate-tbd-against-ir=none -swift-version 5 2>&1 \
// RUN:   | %FileCheck %s
// CHECK: error: reference to top-level declaration 'foo' broken by a context change; the declaration kind of 'foo' from 'A' changed since building 'LibWithXRef'
// CHECK: note: the declaration was expected to be found in module 'A' at '{{.*}}module.modulemap'
// CHECK: note: the declaration was actually found in module 'A' at '{{.*}}module.modulemap'
// CHECK: note: a candidate was filtered out because of a type mismatch; expected: '() -> ()', found: '() -> Float'

//--- module.modulemap
module A {
    header "A.h"
}

//--- A.h
void foo() {}

//--- A_DifferentAPI.h
float foo() {
    return 1.2;
}

//--- LibWithXRef.swift
import A

@inlinable
public func bar() {
    foo()
}

//--- Client.swift
import LibWithXRef

bar()
