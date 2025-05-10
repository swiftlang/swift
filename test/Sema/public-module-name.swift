// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

/// Build the libraries.
// RUN: %target-swift-frontend %t/LibCore.swift \
// RUN:   -emit-module-path %t/LibCore.swiftmodule \
// RUN:   -emit-module-interface-path %t/LibCore.swiftinterface \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -public-module-name Lib
// RUN: %target-swift-typecheck-module-from-interface(%t/LibCore.swiftinterface)

// RUN: %target-swift-frontend %t/LibMiddle.swift -I %t \
// RUN:   -emit-module-path %t/LibMiddle.swiftmodule \
// RUN:   -emit-module-interface-path %t/LibMiddle.swiftinterface \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -public-module-name Lib
// RUN: %target-swift-typecheck-module-from-interface(%t/LibMiddle.swiftinterface) -I %t

// RUN: %target-swift-frontend %t/Lib.swift -I %t \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface \
// RUN:   -enable-library-evolution -swift-version 6
// RUN: %target-swift-typecheck-module-from-interface(%t/Lib.swiftinterface) -I %t

// RUN: %target-swift-frontend %t/LibUnrelated.swift -I %t \
// RUN:   -emit-module-path %t/LibUnrelated.swiftmodule \
// RUN:   -enable-library-evolution -swift-version 6

/// Check flag in swiftinterface
// RUN: cat %t/LibCore.swiftinterface | %FileCheck --check-prefix=CHECK-FLAG %s
// RUN: cat %t/LibMiddle.swiftinterface | %FileCheck --check-prefix=CHECK-FLAG %s
// CHECK-FLAG: swift-module-flags-ignorable:
// CHECK-FLAG-SAME: -public-module-name Lib

/// Build clients against binary swiftmodules.
/// First errors in files, then diagnostics in other files.
// RUN: %target-swift-frontend -typecheck %t/ClientPublic.swift -o %t -I %t \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -verify
// RUN: not %target-swift-frontend -typecheck %t/ClientPublic.swift -o %t -I %t \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -diagnostic-style llvm \
// RUN:   2>&1 | %FileCheck %t/ClientPublic.swift

// RUN: %target-swift-frontend -typecheck %t/ClientMiddle.swift -o %t -I %t \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -verify
// RUN: not %target-swift-frontend -typecheck %t/ClientMiddle.swift -o %t -I %t \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -diagnostic-style llvm \
// RUN:   2>&1 | %FileCheck %t/ClientMiddle.swift

/// Test more diagnostics referencing modules.
// RUN: %target-swift-frontend -typecheck %t/ClientAccessLevelOnImports.swift -o %t -I %t \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -verify

/// Build client against textual swiftinterfaces.
// RUN: rm %t/LibCore.swiftmodule %t/LibMiddle.swiftmodule %t/Lib.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/ClientPublic.swift -I %t \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -verify
// RUN: not %target-swift-frontend -typecheck %t/ClientPublic.swift -o %t -I %t \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -diagnostic-style llvm \
// RUN:   2>&1 | %FileCheck %t/ClientPublic.swift

// RUN: %target-swift-frontend -typecheck %t/ClientMiddle.swift -o %t -I %t \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -verify
// RUN: not %target-swift-frontend -typecheck %t/ClientMiddle.swift -o %t -I %t \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -diagnostic-style llvm \
// RUN:   2>&1 | %FileCheck %t/ClientMiddle.swift

// RUN: %target-swift-frontend -typecheck %t/ClientAccessLevelOnImports.swift -o %t -I %t \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -verify

//--- LibCore.swift
public func ambiguous() {}
public func coreFunc() {}

//--- LibMiddle.swift
import LibCore
public func ambiguous() {}

//--- Lib.swift
@_exported import LibCore
@_exported import LibMiddle

//--- LibUnrelated.swift
public func ambiguous() {}

//--- ClientPublic.swift
import Lib
import LibUnrelated

ambiguous() // expected-error {{ambiguous use of 'ambiguous()'}}
// CHECK-NOT: LibCore
// CHECK-NOT: LibMiddle
// CHECK: LibUnrelated.ambiguous:1:13: note: found this candidate in module 'LibUnrelated'
// CHECK: Lib.ambiguous:1:13: note: found this candidate in module 'Lib'
// CHECK: Lib.ambiguous:1:13: note: found this candidate in module 'Lib'

//--- ClientMiddle.swift
import LibCore
import LibMiddle

ambiguous() // expected-error {{ambiguous use of 'ambiguous()'}}
// CHECK: LibCore.ambiguous:1:13: note: found this candidate in module 'LibCore'
// CHECK: LibMiddle.ambiguous:1:13: note: found this candidate in module 'LibMiddle'

//--- ClientAccessLevelOnImports.swift
internal import Lib // expected-note {{global function 'coreFunc()' imported as 'internal' from 'Lib' here}}

@inlinable
public func foo() {
    coreFunc() // expected-error {{global function 'coreFunc()' is internal and cannot be referenced from an '@inlinable' function}}
}
