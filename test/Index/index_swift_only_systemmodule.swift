import SomeModule
print(someFunc())

// UNIT: Record | system | SomeModule |

// RUN: %empty-directory(%t)
//
// RUN: echo 'public func someFunc() -> Int { return 42; }' >%t/some-module.swift
// RUN: echo 'public struct XWrapper {' >>%t/some-module.swift
// RUN: echo '  public let x: Int' >>%t/some-module.swift
// RUN: echo '  public init(x: Int) {' >>%t/some-module.swift
// RUN: echo '    self.x = x' >>%t/some-module.swift
// RUN: echo '  }' >>%t/some-module.swift
// RUN: echo '}' >>%t/some-module.swift
//
// -----------------------------------------------------------------------------
// --- Prepare SDK (.swiftmodule).
// RUN: %empty-directory(%t/SDK)
// RUN: mkdir -p %t/SDK/Frameworks/SomeModule.framework/Modules/SomeModule.swiftmodule
// RUN: %target-swift-frontend \
// RUN:     -emit-module \
// RUN:     -module-name SomeModule \
// RUN:     -o %t/SDK/Frameworks/SomeModule.framework/Modules/SomeModule.swiftmodule/%module-target-triple.swiftmodule \
// RUN:     -swift-version 5 \
// RUN:     %t/some-module.swift

// -----------------------------------------------------------------------------
// Test-1 - '.swiftmodule' - Normal index-while-building.
//
// RUN: %empty-directory(%t/idx)
// RUN: %empty-directory(%t/modulecache)
//
// --- Built with indexing
// RUN: %target-swift-frontend \
// RUN:     -typecheck \
// RUN:     -index-system-modules \
// RUN:     -index-store-path %t/idx \
// RUN:     -sdk %t/SDK \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     -module-cache-path %t/modulecache \
// RUN:     %s
//
// --- Check the index.
// RUN: c-index-test core -print-unit %t/idx | %FileCheck -check-prefix=UNIT %s

// -----------------------------------------------------------------------------
// --- Prepare SDK (.swiftinterface).
// RUN: %empty-directory(%t/SDK)
// RUN: mkdir -p %t/SDK/Frameworks/SomeModule.framework/Modules/SomeModule.swiftmodule
// RUN: %target-swift-frontend \
// RUN:     -emit-module \
// RUN:     -module-name SomeModule \
// RUN:     -emit-module-interface-path %t/SDK/Frameworks/SomeModule.framework/Modules/SomeModule.swiftmodule/%module-target-triple.swiftinterface \
// RUN:     -o /dev/null \
// RUN:     -swift-version 5 \
// RUN:     -enable-library-evolution \
// RUN:     %t/some-module.swift

// -----------------------------------------------------------------------------
// Test-2 - '.swiftinterface' - Normal index-while-building.
//
// RUN: %empty-directory(%t/idx)
// RUN: %empty-directory(%t/modulecache)
//
// --- Built with indexing
// RUN: %target-swift-frontend \
// RUN:     -typecheck \
// RUN:     -index-system-modules \
// RUN:     -index-store-path %t/idx \
// RUN:     -sdk %t/SDK \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     -module-cache-path %t/modulecache \
// RUN:     %s
//
// --- Check the index.
// RUN: c-index-test core -print-unit %t/idx | %FileCheck -check-prefix=UNIT %s

// -----------------------------------------------------------------------------
// Test-3 - '.swiftinterface' - Build once to populate modulecache, then index-while-building.
//
// RUN: %empty-directory(%t/idx)
// RUN: %empty-directory(%t/modulecache)
//
// --- Build without indexing
// RUN: %target-swift-frontend \
// RUN:     -typecheck \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     -module-cache-path %t/modulecache \
// RUN:     %s
//
// --- Ensure module cache is populated.
// RUN: find %t/modulecache -maxdepth 1 -name 'SomeModule-*.swiftmodule' | grep .
//
// --- Built with indexing
// RUN: %target-swift-frontend \
// RUN:     -typecheck \
// RUN:     -index-system-modules \
// RUN:     -index-store-path %t/idx \
// RUN:     -sdk %t/SDK \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     -module-cache-path %t/modulecache \
// RUN:     %s
//
// --- Check the index.
// RUN: c-index-test core -print-unit %t/idx | %FileCheck -check-prefix=UNIT %s

// -----------------------------------------------------------------------------
// Test-4 - '.swiftinterface' - Prebuild module in prebuilt-module-cache-path
//
// RUN: %empty-directory(%t/idx)
// RUN: %empty-directory(%t/modulecache)
// RUN: %empty-directory(%t/prebuiltcache)
//
// --- Prebuild SDK module.
// RUN: mkdir -p %t/prebuiltcache/SomeModule.swiftmodule
// RUN: %target-swift-frontend \
// RUN:     -compile-module-from-interface \
// RUN:     -module-name SomeModule \
// RUN:     -o %t/prebuiltcache/SomeModule.swiftmodule/%module-target-triple.swiftmodule \
// RUN:     %t/SDK/Frameworks/SomeModule.framework/Modules/SomeModule.swiftmodule/%module-target-triple.swiftinterface
//
// --- Build main file with indexing.
// RUN: %target-swift-frontend \
// RUN:     -typecheck \
// RUN:     -index-system-modules \
// RUN:     -index-store-path %t/idx \
// RUN:     -sdk %t/SDK \
// RUN:     -Fsystem %t/SDK/Frameworks \
// RUN:     -module-cache-path %t/modulecache \
// RUN:     -prebuilt-module-cache-path %t/prebuiltcache \
// RUN:     %s
//
// --- Check the index.
// RUN: c-index-test core -print-unit %t/idx | %FileCheck -check-prefix=UNIT %s
