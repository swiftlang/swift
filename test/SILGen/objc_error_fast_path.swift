// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines
// REQUIRES: objc_interop
// REQUIRES: executable_test

/// Optimization active: public import
// RUN: %target-build-swift-dylib(%t/%target-library-name(Lib)) \
// RUN:   %t/optimized.swift -module-name Lib \
// RUN:   -emit-module -emit-module-path %t/Lib.swiftmodule \
// RUN:   -swift-version 5 -enable-library-evolution -O
// RUN: %target-codesign %t/%target-library-name(Lib)
// RUN: %target-build-swift %t/main.swift -o %t/main -I%t -lLib -L%t -O
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(Lib) | %FileCheck %s

/// Ensure the client has the optimization we're testing here.
// RUN: %target-swift-frontend -typecheck -emit-silgen %t/Lib.swiftmodule > %t/Lib.sil
// RUN: %FileCheck --check-prefix CHECK-OPTIMIZED --input-file %t/Lib.sil %s
// CHECK-OPTIMIZED: _getErrorEmbeddedNSError

/// No optimization: internal import from resilient module
// RUN: %target-build-swift-dylib(%t/%target-library-name(Lib)) \
// RUN:   %t/non-optimized.swift -module-name Lib \
// RUN:   -emit-module -emit-module-path %t/Lib.swiftmodule \
// RUN:   -swift-version 5 -enable-library-evolution -O
// RUN: %target-codesign %t/%target-library-name(Lib)
// RUN: %target-build-swift %t/main.swift -o %t/main -I%t -lLib -L%t -O
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(Lib) | %FileCheck %s

/// Ensure the client doesn't have the optimization we're testing here.
// RUN: %target-swift-frontend -typecheck -emit-silgen %t/main.swift -I%t -O > %t/Lib.sil
// RUN: %FileCheck --check-prefix CHECK-NOT-OPTIMIZED --input-file %t/Lib.sil %s
// CHECK-NOT-OPTIMIZED-NOT: _getErrorEmbeddedNSError

//--- optimized.swift
public import Foundation

@inlinable public func foo<E: Error>(e: E) -> Error {
    return e
}

//--- non-optimized.swift
internal import Foundation

@inlinable public func foo<E: Error>(e: E) -> Error {
    return e
}

//--- main.swift
import Lib
import Foundation

let err: NSError = NSError(domain: "Not found", code: 404)
let errOut: Error = foo(e: err)
print(errOut.localizedDescription)
// CHECK: Not found error 404.
