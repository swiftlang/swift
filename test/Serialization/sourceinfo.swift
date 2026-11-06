import MyModule

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Modules)

// RUN: %target-swiftc_driver -emit-module -module-name MyModule -o %t/Modules/MyModule.swiftmodule %S/Inputs/SourceInfo/File1.swift %S/Inputs/SourceInfo/File2.swift
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print MyModule -enable-swiftsourceinfo -I %t/Modules -source-filename %s | %FileCheck %s --check-prefixes=CHECK,%if parser_astgen %{ASTGEN%} %else %{CPARSER%}

// The C++ parser hashes some tokens twice (e.g. those it skips while looking
// ahead for a declaration), so its hashes differ from ASTGen's.
// CPARSER: filepath=SOURCE_DIR{{[/\\]}}test{{[/\\]}}Serialization{{[/\\]}}Inputs{{[/\\]}}SourceInfo{{[/\\]}}File1.swift; hash=9da710e9b2de1fff2915639236b8929c; hashExcludingTypeMembers=bef81a9bfc04156da679d8d579125d39; mtime={{[0-9]{4}-[0-9]{2}-[0-9]{2} .*}}; size=35
// ASTGEN: filepath=SOURCE_DIR{{[/\\]}}test{{[/\\]}}Serialization{{[/\\]}}Inputs{{[/\\]}}SourceInfo{{[/\\]}}File1.swift; hash=7f5f69ee2483901378dc3be737c3a866; hashExcludingTypeMembers=5337b96a049fc80cc8e4e397ce73cbb6; mtime={{[0-9]{4}-[0-9]{2}-[0-9]{2} .*}}; size=35
// CPARSER: filepath=SOURCE_DIR{{[/\\]}}test{{[/\\]}}Serialization{{[/\\]}}Inputs{{[/\\]}}SourceInfo{{[/\\]}}File2.swift; hash=22b75a7717318d48f7a979906f35195e; hashExcludingTypeMembers=7a22dbe5fc611122201f7d6b53094531; mtime={{[0-9]{4}-[0-9]{2}-[0-9]{2} .*}}; size=57
// ASTGEN: filepath=SOURCE_DIR{{[/\\]}}test{{[/\\]}}Serialization{{[/\\]}}Inputs{{[/\\]}}SourceInfo{{[/\\]}}File2.swift; hash=a4aac44f5e97ba01d2b0da3748102ba9; hashExcludingTypeMembers=be0dc714678203e59d99c7304819233a; mtime={{[0-9]{4}-[0-9]{2}-[0-9]{2} .*}}; size=57
