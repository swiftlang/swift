// RUN: %empty-directory(%t)
// RUN: touch %t/file-01.swift %t/file-02.swift %t/file-03.swift
// RUN: echo 'public func main() {}' >%t/main.swift
// RUN: echo 'extern int foo;' >%t/foo-bridging-header.h
//
// RUN: %swiftc_driver -enable-bridging-pch -v -import-objc-header %t/foo-bridging-header.h -enable-batch-mode -c -emit-module -module-name main -j 2 %t/file-01.swift %t/file-02.swift %t/file-03.swift %t/main.swift %s 2>&1 | %FileCheck %s
//
// CHECK: -emit-pch
// CHECK: -primary-file {{.*}}/file-01.swift -primary-file {{.*}}/file-02.swift

func bar() {
    print(foo)
}
