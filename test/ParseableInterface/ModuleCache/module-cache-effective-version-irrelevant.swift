// RUN: %empty-directory(%t)
//
// Test will build a module TestModule that depends on OtherModule and LeafModule (built from other.swift and leaf.swift).
//
// RUN: echo 'public func LeafFunc() -> Int { return 10; }' >%t/leaf.swift
//
// RUN: echo 'import LeafModule' >%t/other.swift
// RUN: echo 'public func OtherFunc() -> Int { return LeafFunc(); }' >>%t/other.swift
//
// Phase 1: build LeafModule into a .swiftinterface file with -swift-version 4:
//
// RUN: %target-swift-frontend -swift-version 4 -I %t -module-cache-path %t/modulecache -emit-parseable-module-interface-path %t/LeafModule.swiftinterface -module-name LeafModule %t/leaf.swift -typecheck
//
// Phase 2: build OtherModule into a .swiftinterface file with -swift-version 4.2:
//
// RUN: %target-swift-frontend -swift-version 4.2 -I %t -module-cache-path %t/modulecache -emit-parseable-module-interface-path %t/OtherModule.swiftinterface -module-name OtherModule %t/other.swift -enable-parseable-module-interface -typecheck
//
// Phase 3: build TestModule in -swift-version 5 and import both of these:
//
// RUN: %target-swift-frontend -swift-version 5 -I %t -module-cache-path %t/modulecache -module-name TestModule %s -enable-parseable-module-interface -typecheck
//
// Phase 4: make sure we only compiled LeafModule and OtherModule one time:
//
// RUN: NUM_LEAF_MODULES=$(find %t/modulecache -type f -name 'LeafModule-*.swiftmodule' | wc -l)
// RUN: NUM_OTHER_MODULES=$(find %t/modulecache -type f -name 'OtherModule-*.swiftmodule' | wc -l)
// RUN: if [ ! $NUM_LEAF_MODULES -eq 1 ]; then echo "Should only be 1 LeafModule, found $NUM_LEAF_MODULES"; exit 1; fi
// RUN: if [ ! $NUM_OTHER_MODULES -eq 1 ]; then echo "Should only be 1 OtherModule, found $NUM_OTHER_MODULES"; exit 1; fi
import LeafModule
import OtherModule