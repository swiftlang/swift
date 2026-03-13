// UNSUPPORTED: OS=windows-msvc
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %t -emit-module-interface-path %t/LeafModule.swiftinterface -module-name LeafModule %t/leaf.swift -emit-module -o /dev/null
// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %t -module-cache-path %t/swiftcache -emit-module-interface-path %t/OtherModule.swiftinterface -module-name OtherModule %t/other.swift -emit-module -o /dev/null
// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %t -module-cache-path %t/swiftcache -Xcc -fmodules-cache-path=%t/clangcache -emit-module -o %t/TestModule.swiftmodule -module-name TestModule %t/main.swift

/// Two swift modules, Leaf and Other
// RUN: %find_files %t/swiftcache '*.swiftmodule' | %llvm_obj_root/bin/count 2
/// Two clang modules, shim and A
// RUN: %find_files %t/clangcache '*.pcm' | %llvm_obj_root/bin/count 2


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
