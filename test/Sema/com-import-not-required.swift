// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop -com-interop-model=microsoft %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-com-interop -com-interop-model=microsoft -disable-implicit-com-module-import -I %t -primary-file %s %S/Inputs/com_importer.swift

// The implementation identity no longer synthesizes a declaration using the
// COM module's CLSID type. The attribute therefore does not require COM to be
// imported by this source file when another file has already loaded it.

@com(implementation: "AABBCCDD-EEFF-0011-2233-445566778899")
class Widget {}
