import MyModule

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Modules)

// RUN: %target-swiftc_driver -emit-module -module-name MyModule -o %t/Modules/MyModule.swiftmodule %S/Inputs/SourceInfo/File1.swift %S/Inputs/SourceInfo/File2.swift
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print MyModule -enable-swiftsourceinfo -I %t/Modules -source-filename %s | %FileCheck %s

// CHECK: filepath=SOURCE_DIR{{[/\\]}}test{{[/\\]}}Serialization{{[/\\]}}Inputs{{[/\\]}}SourceInfo{{[/\\]}}File1.swift; hash=b44bab617797a7239a9fa948f11eb90b; mtime={{[0-9]{4}-[0-9]{2}-[0-9]{2} .*}}; size=35
// CHECK: filepath=SOURCE_DIR{{[/\\]}}test{{[/\\]}}Serialization{{[/\\]}}Inputs{{[/\\]}}SourceInfo{{[/\\]}}File2.swift; hash=c989d6b98d505a1f52749d43ea0569a1; mtime={{[0-9]{4}-[0-9]{2}-[0-9]{2} .*}}; size=57
