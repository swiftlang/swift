// This test specifically uses macOS deployment targets
// REQUIRES: OS=macosx
//
// RUN: %empty-directory(%t)
//
// Test will build a module TestModule that depends on OtherModule and LeafModule (built from other.swift and leaf.swift).
//
// RUN: echo 'public func LeafFunc() -> Int { return 10; }' >%t/leaf.swift
//
// RUN: echo 'import LeafModule' >%t/other.swift
// RUN: echo 'public func OtherFunc() -> Int { return LeafFunc(); }' >>%t/other.swift
//
// Phase 1: build LeafModule into a .swiftinterface file with -target %target-cpu-macosx-10.9:
//
// RUN: %swift -target %target-cpu-apple-macosx10.9 -I %t -module-cache-path %t/modulecache -emit-module-interface-path %t/LeafModule.swiftinterface -module-name LeafModule %t/leaf.swift -typecheck
//
// Phase 2: build OtherModule into a .swiftinterface file with -target %target-cpu-macosx-10.10:
//
// RUN: %swift -target %target-cpu-apple-macosx10.10 -I %t -module-cache-path %t/modulecache -emit-module-interface-path %t/OtherModule.swiftinterface -module-name OtherModule %t/other.swift -typecheck
//
// Phase 3: build TestModule in -target %target-cpu-apple-macosx10.11 and import both of these:
//
// RUN: %swift -target %target-cpu-apple-macosx10.11  -I %t -module-cache-path %t/modulecache -module-name TestModule %s -typecheck
//
// Phase 4: make sure we only compiled LeafModule and OtherModule one time:
//
// RUN: %find_files %t/modulecache 'LeafModule-*.swiftmodule' | %llvm_obj_root/bin/count 1
// RUN: %find_files %t/modulecache 'OtherModule-*.swiftmodule' | %llvm_obj_root/bin/count 1
import LeafModule
import OtherModule
