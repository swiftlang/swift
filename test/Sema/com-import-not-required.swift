// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -typecheck -enable-experimental-com-interop -com-interop-model=microsoft -disable-implicit-com-module-import -I %t -primary-file %s %S/Inputs/com-importer.swift

// `@com` identities are stored in descriptors and exposed through metatype
// witnesses. The attribute does not synthesize a declaration that requires
// COM types to be visible in this source file.

@com(implementation: "AABBCCDD-EEFF-0011-2233-445566778899")
class Widget {}
