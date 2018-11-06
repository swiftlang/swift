// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/modulecache)
//
// Setup builds a module TestModule that depends on OtherModule and LeafModule (built from other.swift and leaf.swift).
// During setup, input and intermediate mtimes are all set to a constant 'old' time (long in the past).
//
// We then modify OtherModule.swiftinterface, and check only OtherModule-*.swiftmodule has a new mtime, LeafModule is unchanged.
//
//
// Setup phase 1: Write input files.
//
// RUN: echo 'public func LeafFunc() -> Int { return 10; }' >%t/leaf.swift
//
// RUN: echo 'import LeafModule' >%t/other.swift
// RUN: echo 'public func OtherFunc() -> Int { return LeafFunc(); }' >>%t/other.swift
//
//
// Setup phase 2: build modules, pushing timestamps of inputs and intermediates into the past as we go.
//
// RUN: %S/Inputs/make-old.py %t/leaf.swift %t/other.swift
// RUN: %target-swift-frontend -I %t -emit-parseable-module-interface-path %t/LeafModule.swiftinterface -module-name LeafModule %t/leaf.swift -emit-module -o /dev/null
// RUN: %S/Inputs/make-old.py %t/LeafModule.swiftinterface
// RUN: %target-swift-frontend -I %t -module-cache-path %t/modulecache -enable-parseable-module-interface -emit-parseable-module-interface-path %t/OtherModule.swiftinterface -module-name OtherModule %t/other.swift -emit-module -o /dev/null
// RUN: %S/Inputs/make-old.py %t/modulecache/LeafModule-*.swiftmodule %t/OtherModule.swiftinterface
// RUN: %target-swift-frontend -I %t -module-cache-path %t/modulecache -enable-parseable-module-interface -emit-module -o %t/TestModule.swiftmodule -module-name TestModule %s
// RUN: %S/Inputs/make-old.py %t/modulecache/OtherModule-*.swiftmodule
//
//
// Actual test: make OtherModule.swiftinterface newer, check we only rebuild its cached module.
//
// RUN: %S/Inputs/check-is-old.py %t/OtherModule.swiftinterface %t/LeafModule.swiftinterface
// RUN: %S/Inputs/check-is-old.py %t/modulecache/OtherModule-*.swiftmodule %t/modulecache/LeafModule-*.swiftmodule
// RUN: touch %t/OtherModule.swiftinterface
// RUN: %S/Inputs/check-is-new.py %t/OtherModule.swiftinterface
// RUN: rm %t/TestModule.swiftmodule
// RUN: %target-swift-frontend -I %t -module-cache-path %t/modulecache -enable-parseable-module-interface -emit-module -o %t/TestModule.swiftmodule -module-name TestModule %s
// RUN: %S/Inputs/check-is-new.py %t/modulecache/OtherModule-*.swiftmodule
// RUN: %S/Inputs/check-is-old.py %t/modulecache/LeafModule-*.swiftmodule

import OtherModule

public func TestFunc() {
    print(OtherFunc())
}
