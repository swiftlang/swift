// BEGIN File1.swift
public func foo(val: MyStruct) {
}

// BEGIN File2.swift
public struct MyStruct {
    var x: Int
    var y: Int
}

// BEGIN DUMMY.swift
import MyModule

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Modules)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swiftc_driver -emit-module -module-name MyModule -o %t/Modules/MyModule.swiftmodule %t/File1.swift %t/File2.swift
// RUN: %swift-ide-test -print-module-metadata -module-to-print MyModule -enable-swiftsourceinfo -I %t/Modules -source-filename %t/DUMMY.swift | %FileCheck %s

// CHECK: filepath=BUILD_DIR/{{.*}}/File1.swift; hash=b44bab617797a7239a9fa948f11eb90b; mtime={{[0-9]{4}-[0-9]{2}-[0-9]{2} .*}}; size=36
// CHECK: filepath=BUILD_DIR/{{.*}}/File2.swift; hash=c989d6b98d505a1f52749d43ea0569a1; mtime={{[0-9]{4}-[0-9]{2}-[0-9]{2} .*}}; size=58
