// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Setup the SDK composed of SecretModule, SystemModule, SystemDepA, SystemDepB, and SystemDepCommon
// RUN: %empty-directory(%t/SDK)
// RUN: mkdir -p %t/SDK/Frameworks/SecretModule.framework/Modules/SecretModule.swiftmodule
// RUN: %target-swift-frontend -emit-module -module-name SecretModule \
// RUN:     -swift-version 5 -enable-library-evolution -parse-stdlib \
// RUN:     -o %t/SDK/Frameworks/SecretModule.framework/Modules/SecretModule.swiftmodule/%module-target-triple.swiftmodule \
// RUN:     -emit-module-interface-path %t/SDK/Frameworks/SecretModule.framework/Modules/SecretModule.swiftmodule/%module-target-triple.swiftinterface \
// RUN:     %t/SecretModule.swift

// RUN: mkdir -p %t/SDK/Frameworks/SystemDepCommon.framework/Modules/SystemDepCommon.swiftmodule
// RUN: %target-swift-frontend -emit-module -module-name SystemDepCommon \
// RUN:     -swift-version 5 -enable-library-evolution -parse-stdlib \
// RUN:     -o %t/SDK/Frameworks/SystemDepCommon.framework/Modules/SystemDepCommon.swiftmodule/%module-target-triple.swiftmodule \
// RUN:     -emit-module-interface-path %t/SDK/Frameworks/SystemDepCommon.framework/Modules/SystemDepCommon.swiftmodule/%module-target-triple.swiftinterface \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     %t/SystemDepCommon.swift

// RUN: mkdir -p %t/SDK/Frameworks/SystemDepA.framework/Modules/SystemDepA.swiftmodule
// RUN: %target-swift-frontend -emit-module -module-name SystemDepA \
// RUN:     -swift-version 5 -enable-library-evolution -parse-stdlib \
// RUN:     -o %t/SDK/Frameworks/SystemDepA.framework/Modules/SystemDepA.swiftmodule/%module-target-triple.swiftmodule \
// RUN:     -emit-module-interface-path %t/SDK/Frameworks/SystemDepA.framework/Modules/SystemDepA.swiftmodule/%module-target-triple.swiftinterface \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     %t/SystemDepA.swift

// RUN: mkdir -p %t/SDK/Frameworks/SystemDepB.framework/Modules/SystemDepB.swiftmodule
// RUN: %target-swift-frontend -emit-module -module-name SystemDepB \
// RUN:     -swift-version 5 -enable-library-evolution -parse-stdlib \
// RUN:     -o %t/SDK/Frameworks/SystemDepB.framework/Modules/SystemDepB.swiftmodule/%module-target-triple.swiftmodule \
// RUN:     -emit-module-interface-path %t/SDK/Frameworks/SystemDepB.framework/Modules/SystemDepB.swiftmodule/%module-target-triple.swiftinterface \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     %t/SystemDepB.swift

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
// RUN: %target-swift-frontend -typecheck -parse-stdlib -swift-version 5 \
// RUN:     -index-system-modules \
// RUN:     -index-store-path %t/idx \
// RUN:     -index-ignore-stdlib \
// RUN:     -sdk %t/SDK \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     -module-cache-path %t/modulecache \
// RUN:     -Rindexing-system-module -Rmodule-loading \
// RUN:     %t/Client.swift -disable-deserialization-recovery \
// RUN:     2>&1 | %FileCheck -check-prefix=SYSTEM-INDEX %s
// SYSTEM-INDEX: loaded module 'SystemDepCommon'; source: '{{.*}}SystemDepCommon.swiftmodule{{.*}}.swiftinterface'
// SYSTEM-INDEX: loaded module 'SystemDepCommon'; source: '{{.*}}SystemDepCommon.swiftmodule{{.*}}.swiftinterface'
// SYSTEM-INDEX-NOT: loaded module {{.*}}SystemDepCommon
// SYSTEM-INDEX: indexing system module {{.*}}SystemDepCommon.swiftmodule{{.*}}.swiftinterface
// SYSTEM-INDEX-NOT: loaded module {{.*}}SystemDepCommon
// SYSTEM-INDEX-NOT: indexing system module {{.*}}SystemDepCommon

/// The index should have the public API of SystemModule
// RUN: c-index-test core -print-unit %t/idx | %FileCheck -check-prefix=UNIT %s
// UNIT: Unit | system | SystemModule |
// UNIT: Record | system | SystemModule |
// RUN: c-index-test core -print-record %t/idx | %FileCheck -check-prefix=RECORD %s
// RECORD: function/Swift | systemFunc()

/// Now rebuild the same module. We should not reload or re-index any of the
/// system modules.
// RUN: %target-swift-frontend -typecheck -parse-stdlib -swift-version 5 \
// RUN:     -index-system-modules \
// RUN:     -index-store-path %t/idx \
// RUN:     -index-ignore-stdlib \
// RUN:     -sdk %t/SDK \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     -module-cache-path %t/modulecache \
// RUN:     -Rindexing-system-module -Rmodule-loading \
// RUN:     %t/Client.swift -disable-deserialization-recovery \
// RUN:     2>&1 | %FileCheck -check-prefix=SECOND-SYSTEM-INDEX %s
// SECOND-SYSTEM-INDEX: loaded module 'SystemDepCommon'; source: '{{.*}}SystemDepCommon.swiftmodule{{.*}}.swiftinterface'
// SECOND-SYSTEM-INDEX-NOT: loaded module {{.*}}SystemDepCommon
// SECOND-SYSTEM-INDEX-NOT: indexing system module {{.*}}SystemDepCommon

// RUN: c-index-test core -print-unit %t/idx | %FileCheck -check-prefix=UNIT %s
// RUN: c-index-test core -print-record %t/idx | %FileCheck -check-prefix=RECORD %s

/// Rebuild again, but this time remove the index. We should re-index, but not
/// reload the module as it was already built.
// RUN: %empty-directory(%t/idx)
// RUN: %target-swift-frontend -typecheck -parse-stdlib -swift-version 5 \
// RUN:     -index-system-modules \
// RUN:     -index-store-path %t/idx \
// RUN:     -index-ignore-stdlib \
// RUN:     -sdk %t/SDK \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     -module-cache-path %t/modulecache \
// RUN:     -Rindexing-system-module -Rmodule-loading \
// RUN:     %t/Client.swift -disable-deserialization-recovery \
// RUN:     2>&1 | %FileCheck -check-prefix=THIRD-SYSTEM-INDEX %s
// THIRD-SYSTEM-INDEX: loaded module 'SystemDepCommon'; source: '{{.*}}SystemDepCommon.swiftmodule{{.*}}.swiftinterface'
// THIRD-SYSTEM-INDEX-NOT: loaded module {{.*}}SystemDepCommon
// THIRD-SYSTEM-INDEX: indexing system module {{.*}}SystemDepCommon.swiftmodule{{.*}}.swiftinterface
// THIRD-SYSTEM-INDEX-NOT: loaded module {{.*}}SystemDepCommon
// THIRD-SYSTEM-INDEX-NOT: indexing system module {{.*}}SystemDepCommon

// RUN: c-index-test core -print-unit %t/idx | %FileCheck -check-prefix=UNIT %s
// RUN: c-index-test core -print-record %t/idx | %FileCheck -check-prefix=RECORD %s

/// Index a client reading from a broken swiftinterface
// RUN: %empty-directory(%t/idx)
// RUN: %empty-directory(%t/modulecache)
// RUN: echo "breaking_the_swiftinterface" >> %t/SDK/Frameworks/SystemModule.framework/Modules/SystemModule.swiftmodule/%module-target-triple.swiftinterface

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

/// We don't expect to see the swiftinterface error for indexing
// BROKEN-BUILD-NOT: error
// BROKEN-BUILD-NOT: breaking_the_swiftinterface
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
// Both these import SystemDepCommon - we want to check that it is only indexed
// once.
import SystemDepA
import SystemDepB
public func systemFunc() {}
func leakyFunc(_ a: SecretType) {}

// Currently requires salvaging, which we need to make sure runs when the
// interface is rebuilt (as it produces a solution), we'll crash if it isn't.
public struct SysA { public init() {} }
public struct SysB { public init() {} }
@available(macOS, unavailable)
public func forceDisjunction() -> SysA { return SysA() }
public func forceDisjunction() -> SysB { return SysB() }
@available(macOS, unavailable)
@inlinable
public func requireSalvage() -> SysA {
  return forceDisjunction()
}

//--- SystemDepA.swift
import SystemDepCommon
public func systemDepAFunc() {}

//--- SystemDepB.swift
import SystemDepCommon
public func systemDepBFunc() {}

//--- SystemDepCommon.swift
public func systemDepCommonFunc() {}

//--- Client.swift
import SystemModule

public func clientFunc() {}

//--- ClientWithError.swift
import SystemModule
public func clientFunc() {}

struct T {}
struct U {}
func f() -> T { return U() }
