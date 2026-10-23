/// A serialized SIL body referencing a decl from a module that is no longer
/// loaded by clients is diagnosed instead of aborting the compiler.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module \
// RUN:   %t/C.swift -module-name C -o %t/C.swiftmodule -I %t -swift-version 5
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module \
// RUN:   %t/B.swift -module-name B -o %t/B.swiftmodule -I %t -swift-version 5
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module \
// RUN:   %t/Lib.swift -module-name Lib -o %t/Lib.swiftmodule -I %t \
// RUN:   -swift-version 5

/// Build client fine in the expected behavior.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -c -Onone \
// RUN:   %t/Client.swift -I %t -swift-version 5 -o %t/Client.o

/// Rebuild B without re-exporting C, so clients of Lib no longer load C.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module \
// RUN:   %t/B_Changed.swift -module-name B -o %t/B.swiftmodule -I %t \
// RUN:   -swift-version 5
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -c -Onone \
// RUN:   %t/Client.swift -I %t -swift-version 5 -o %t/Client.o \
// RUN:   -diagnostic-style llvm 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -c -O \
// RUN:   %t/Client.swift -I %t -swift-version 5 -o %t/Client.o \
// RUN:   -diagnostic-style llvm 2>&1 | %FileCheck %s

// REQUIRES: objc_interop

// CHECK: error: cannot deserialize the body of '{{.*}}f(_:)' from module 'Lib'; it references module 'C', which is not loaded
// CHECK: note: module 'Lib' may depend on 'C' through an import that clients don't load; import 'C' explicitly to work around this
// CHECK-NOT: *** DESERIALIZATION FAILURE ***

//--- C.swift
import Foundation

public class K: NSObject {
  @objc public func bar() {}
}

//--- B.swift
@_exported import C

//--- B_Changed.swift
public struct Unrelated {}

//--- Lib.swift
import B

@_alwaysEmitIntoClient
public func f(_ o: AnyObject) {
  o.bar?()
}

//--- Client.swift
import Lib

public func g(_ o: AnyObject) { f(o) }
