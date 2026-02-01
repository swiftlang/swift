/// Recover from typical modularization issues seen in clang modules.
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Setup two library modules A and B, and a client.
// RUN: cp %t/LibOriginal.h %t/A.h
// RUN: cp %t/Empty.h %t/B.h
// RUN: %target-swift-frontend %t/LibWithXRef.swift -I %t \
// RUN:   -emit-module-path %t/LibWithXRef.swiftmodule

/// Move MyType from A to B, then error on client.
// RUN: cp %t/Empty.h %t/A.h
// RUN: cp %t/LibOriginal.h %t/B.h
// RUN: not %target-swift-frontend -emit-sil %t/LibWithXRef.swiftmodule -I %t \
// RUN:   -diagnostic-style llvm \
// RUN:   -disable-workaround-broken-modules 2>&1 \
// RUN:   | %FileCheck --check-prefixes CHECK,CHECK-MOVED %s
// CHECK-MOVED: LibWithXRef.swiftmodule:1:1: error: reference to type 'MyType' broken by a context change; 'MyType' was expected to be in 'A', but now a candidate is found only in 'B'

/// Working around the broken modularization to get a result and no errors.
// RUN: %target-swift-frontend -emit-sil %t/LibWithXRef.swiftmodule -I %t \
// RUN:   -diagnostic-style llvm 2>&1 \
// RUN:   | %FileCheck --check-prefixes CHECK-WORKAROUND %s
// CHECK-WORKAROUND: LibWithXRef.swiftmodule:1:1: warning: reference to type 'MyType' broken by a context change; 'MyType' was expected to be in 'A', but now a candidate is found only in 'B'
// CHECK-WORKAROUND-NEXT: A.MyType
// CHECK-WORKAROUND-NEXT: ^
// CHECK-WORKAROUND: note: the type was expected to be found in module 'A' at '
// CHECK-WORKAROUND-SAME: module.modulemap'
// CHECK-WORKAROUND: note: the type was actually found in module 'B' at '
// CHECK-WORKAROUND-SAME: module.modulemap'
// CHECK-WORKAROUND: note: attempting to recover from the previous modularization issue
// CHECK-WORKAROUND: func foo() -> some Proto

/// Change MyType into a function, then error.
// RUN: cp %t/LibTypeChanged.h %t/A.h
// RUN: cp %t/Empty.h %t/B.h
// RUN: not %target-swift-frontend -emit-sil %t/LibWithXRef.swiftmodule -I %t \
// RUN:   -diagnostic-style llvm 2>&1 \
// RUN:   | %FileCheck --check-prefixes CHECK,CHECK-KIND-CHANGED %s
// CHECK-KIND-CHANGED: LibWithXRef.swiftmodule:1:1: error: reference to type 'MyType' broken by a context change; the declaration kind of 'MyType' from 'A' changed since building 'LibWithXRef'

/// Change MyType into a function, move it and then error.
// RUN: cp %t/Empty.h %t/A.h
// RUN: cp %t/LibTypeChanged.h %t/B.h
// RUN: not %target-swift-frontend -emit-sil %t/LibWithXRef.swiftmodule -I %t \
// RUN:   -diagnostic-style llvm 2>&1 \
// RUN:   | %FileCheck --check-prefixes CHECK,CHECK-KIND-CHANGED-AND-MOVED %s
// CHECK-KIND-CHANGED-AND-MOVED: LibWithXRef.swiftmodule:1:1: error: reference to type 'MyType' broken by a context change; the declaration kind of 'MyType' changed since building 'LibWithXRef', it was in 'A' and now a candidate is found only in 'B'

/// Remove MyType from all imported modules and error.
// RUN: cp %t/Empty.h %t/A.h
// RUN: cp %t/Empty.h %t/B.h
// RUN: not %target-swift-frontend -emit-sil %t/LibWithXRef.swiftmodule -I %t \
// RUN:   -diagnostic-style llvm 2>&1 \
// RUN:   | %FileCheck --check-prefixes CHECK,CHECK-NOT-FOUND %s
// CHECK-NOT-FOUND: LibWithXRef.swiftmodule:1:1: error: reference to type 'MyType' broken by a context change; 'MyType' is not found, it was expected to be in 'A'

// CHECK: LibWithXRef.swiftmodule:1:1: note: could not deserialize extension

//--- module.modulemap
module A {
    header "A.h"
}

module B {
    header "B.h"
}

//--- Empty.h

//--- LibOriginal.h
struct MyType {};

//--- LibTypeChanged.h
/// Make it a function to fail filtering.
void MyType() {}

//--- LibWithXRef.swift
import A
import B

public protocol Proto {}
extension MyType : Proto {}

@available(SwiftStdlib 5.1, *)
public func foo() -> some Proto { return MyType() }
