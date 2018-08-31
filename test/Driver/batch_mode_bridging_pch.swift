// RUN: %empty-directory(%t)
// RUN: touch %t/file-01.swift %t/file-02.swift %t/file-03.swift
// RUN: echo 'public func main() {}' >%t/main.swift
// RUN: echo 'extern int foo;' >%t/foo-bridging-header.h
//
// RUN: %swiftc_driver -enable-bridging-pch -v -import-objc-header %t/foo-bridging-header.h -enable-batch-mode -c -emit-module -module-name main -j 2 %t/file-01.swift %t/file-02.swift %t/file-03.swift %t/main.swift %s 2>&1 | %FileCheck %s
//
// Next we make a module map with an unknown attribute, which will cause an
// AST-reader warning while (re)parsing the module map, while attaching a PCH.
// We turn on serialized diagnostics in the frontends, and check that that
// warning, issued before the batch-mode multi-file diagnostic multiplexor has
// its file mappings established, does not crash the multiplexor.
//
// RUN: %empty-directory(%t/MyModule)
// RUN: echo 'module MyModule [DefinitelyNotAnAttribute] { header "header.h" export * }' >%t/MyModule/module.modulemap
// RUN: touch %t/MyModule/header.h
// RUN: echo '#include "MyModule/header.h"' >>%t/foo-bridging-header.h
// RUN: %swiftc_driver -enable-bridging-pch -v -I %t -import-objc-header %t/foo-bridging-header.h -enable-batch-mode -c -emit-module -module-name main -j 2 %t/file-01.swift %t/file-02.swift %t/file-03.swift %t/main.swift -serialize-diagnostics %s 2>&1 | %FileCheck %s
//
// CHECK: -emit-pch
// CHECK: -primary-file {{.*}}/file-01.swift -primary-file {{.*}}/file-02.swift

func bar() {
    print(foo)
}
