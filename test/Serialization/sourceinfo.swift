import MyModule

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Modules)

// RUN: %target-swiftc_driver -emit-module -module-name MyModule -o %t/Modules/MyModule.swiftmodule %S/Inputs/SourceInfo/File1.swift %S/Inputs/SourceInfo/File2.swift
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print MyModule -enable-swiftsourceinfo -I %t/Modules -source-filename %s | %FileCheck %s

// CHECK: filepath=SOURCE_DIR{{[/\\]}}test{{[/\\]}}Serialization{{[/\\]}}Inputs{{[/\\]}}SourceInfo{{[/\\]}}File1.swift; hash=9da710e9b2de1fff2915639236b8929c; hashExcludingTypeMembers=bef81a9bfc04156da679d8d579125d39; mtime={{[0-9]{4}-[0-9]{2}-[0-9]{2} .*}}; size=35
// CHECK: filepath=SOURCE_DIR{{[/\\]}}test{{[/\\]}}Serialization{{[/\\]}}Inputs{{[/\\]}}SourceInfo{{[/\\]}}File2.swift; hash=22b75a7717318d48f7a979906f35195e; hashExcludingTypeMembers=7a22dbe5fc611122201f7d6b53094531; mtime={{[0-9]{4}-[0-9]{2}-[0-9]{2} .*}}; size=57
