// UNSUPPORTED: OS=windows-msvc
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %t -emit-module-interface-path %t/LeafModule.swiftinterface -module-name LeafModule %t/leaf.swift -emit-module -o /dev/null
// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %t -module-cache-path %t/swiftcache -emit-module-interface-path %t/OtherModule.swiftinterface -module-name OtherModule %t/other.swift -emit-module -o /dev/null
// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %t -module-cache-path %t/swiftcache -Xcc -fmodules-cache-path=%t/clangcache -emit-module -o %t/TestModule.swiftmodule -module-name TestModule %t/main.swift

// RUN: NUM_SWIFT_MODULES=$(find %t/swiftcache -type f -name '*.swiftmodule' | wc -l)
// RUN: NUM_CLANG_MODULES=$(find %t/clangcache -type f -name '*.pcm' | wc -l)
/// Two swift modules, Leaf and Other
// RUN: if [ ! $NUM_SWIFT_MODULES -eq 2 ]; then echo "Should only be 2 Swift Modules, found $NUM_SWIFT_MODULES"; exit 1; fi
/// Two clang modules, shim and A
// RUN: if [ ! $NUM_CLANG_MODULES -eq 2 ]; then echo "Should only be 2 Clang Modules, found $NUM_CLANG_MODULES"; exit 1; fi

//--- leaf.swift
public func LeafFunc() {}

//--- other.swift
import LeafModule
public func OtherFunc() {}

//--- module.modulemap
module A {
  header "A.h"
  export *
}

//--- A.h
void a(void);

//--- main.swift
import OtherModule
import A

public func TestFunc() {
    OtherFunc()
    a()
}
