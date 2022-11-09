// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/modulecache)
// RUN: split-file %s %t

/// Setup the SDK composed of SecretModule and SystemModule
// RUN: %empty-directory(%t/SDK)
// RUN: mkdir -p %t/SDK/Frameworks/SecretModule.framework/Modules/SecretModule.swiftmodule
// RUN: %target-swift-frontend -emit-module -module-name SecretModule \
// RUN:     -swift-version 5 -enable-library-evolution -parse-stdlib \
// RUN:     -o %t/SDK/Frameworks/SecretModule.framework/Modules/SecretModule.swiftmodule/%module-target-triple.swiftmodule \
// RUN:     -emit-module-interface-path %t/SDK/Frameworks/SecretModule.framework/Modules/SecretModule.swiftmodule/%module-target-triple.swiftinterface \
// RUN:     %t/SecretModule.swift
// RUN: mkdir -p %t/SDK/Frameworks/SystemModule.framework/Modules/SystemModule.swiftmodule
// RUN: %target-swift-frontend -emit-module -module-name SystemModule \
// RUN:     -swift-version 5 -enable-library-evolution -parse-stdlib \
// RUN:     -o %t/SDK/Frameworks/SystemModule.framework/Modules/SystemModule.swiftmodule/%module-target-triple.swiftmodule \
// RUN:     -emit-module-interface-path %t/SDK/Frameworks/SystemModule.framework/Modules/SystemModule.swiftmodule/%module-target-triple.swiftinterface \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     %t/SystemModule.swift

/// Index a client of SystemModule reading from the swiftinterface.
/// Because of disable-deserialization-recovery and leakyFunc, reading from
/// the swiftmodule would crash.
// RUN: %empty-directory(%t/idx)
// RUN: %target-swift-frontend -typecheck -parse-stdlib -swift-version 5 \
// RUN:     -index-system-modules \
// RUN:     -index-store-path %t/idx \
// RUN:     -index-ignore-stdlib \
// RUN:     -sdk %t/SDK \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     -module-cache-path %t/modulecache \
// RUN:     %t/Client.swift -disable-deserialization-recovery

/// The index should have the public API of SystemModule
// RUN: c-index-test core -print-unit %t/idx | %FileCheck -check-prefix=UNIT %s
// UNIT: Unit | system | SystemModule |
// UNIT: Record | system | SystemModule |
// RUN: c-index-test core -print-record %t/idx | %FileCheck -check-prefix=RECORD %s
// RECORD: function/Swift | systemFunc()

/// Index a client reading from a broken swiftinterface
// RUN: %empty-directory(%t/idx)
// RUN: %empty-directory(%t/modulecache)
// RUN: echo "breaking_the_swifinterface" >> %t/SDK/Frameworks/SystemModule.framework/Modules/SystemModule.swiftmodule/%module-target-triple.swiftinterface

// RUN: %target-swift-frontend -typecheck -parse-stdlib \
// RUN:     -index-system-modules \
// RUN:     -index-store-path %t/idx \
// RUN:     -index-ignore-stdlib \
// RUN:     -sdk %t/SDK \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     -module-cache-path %t/modulecache \
// RUN:     -Rindexing-system-module \
// RUN:     %t/Client.swift \
// RUN:     2>&1 | %FileCheck -check-prefix=BROKEN-BUILD %s

/// We don't expect so see the swiftinterface error for indexing
// BROKEN-BUILD-NOT: error
// BROKEN-BUILD-NOT: breaking_the_swifinterface
// BROKEN-BUILD: indexing system module {{.*}} skipping

/// We don't expect SystemModule to be indexed with a broken swiftinterface
// RUN: c-index-test core -print-unit %t/idx | %FileCheck -check-prefix=BROKEN-UNIT %s
// BROKEN-UNIT: Unit | system | SystemModule |
// BROKEN-UNIT-NOT: Record | system | SystemModule |
// RUN: c-index-test core -print-record %t/idx | %FileCheck -check-prefix=BROKEN-RECORD %s
// BROKEN-RECORD-NOT: function/Swift | systemFunc()

/// Subsequent builds won't attempt to index the broken swiftinterface again
// RUN: %target-swift-frontend -typecheck -parse-stdlib \
// RUN:     -index-system-modules \
// RUN:     -index-store-path %t/idx \
// RUN:     -index-ignore-stdlib \
// RUN:     -sdk %t/SDK \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     -module-cache-path %t/modulecache \
// RUN:     -Rindexing-system-module \
// RUN:     %t/Client.swift \
// RUN:     2>&1 | %FileCheck -check-prefix=BROKEN-BUILD-2 --allow-empty %s
// BROKEN-BUILD-2-NOT: indexing system module

/// Local errors should be preserved even when indexing against a broken swiftinterface
// RUN: %empty-directory(%t/idx)
// RUN: %empty-directory(%t/modulecache)
// RUN: not %target-swift-frontend -typecheck -parse-stdlib \
// RUN:     -index-system-modules \
// RUN:     -index-store-path %t/idx \
// RUN:     -index-ignore-stdlib \
// RUN:     -sdk %t/SDK \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     -module-cache-path %t/modulecache \
// RUN:     %t/ClientWithError.swift 2> %t/client-with-error.err
// RUN: cat %t/client-with-error.err | %FileCheck -check-prefix=WITH-ERROR %s
// WITH-ERROR: cannot convert return expression of type 'U' to return type 'T'

//--- SecretModule.swift
public struct SecretType {}

//--- SystemModule.swift
// Use this dependency to hit an easy deserialization failure when recovery is disabled.
@_implementationOnly import SecretModule

public func systemFunc() { }
func leakyFunc(_ a: SecretType) { }

//--- Client.swift
import SystemModule

public func clientFunc() {}

//--- ClientWithError.swift

import SystemModule
public func clientFunc() {}

struct T {}
struct U {}
func f() -> T { return U() }
