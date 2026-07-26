// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-com-interop -disable-implicit-com-module-import -I %t -primary-file %s %S/Inputs/com_importer.swift

// `@com` ID synthesis references the COM module's `CLSID`/`IID` types, so it
// must require that COM is imported by *this* file -- not merely loaded because
// another file (Inputs/com_importer.swift) imports it. This file does not
// import COM, so synthesizing `Widget`'s `CLSID` must be diagnosed.

@com(implementation: "AABBCCDD-EEFF-0011-2233-445566778899")
class Widget {}
// expected-error@-1 {{'COM' module not imported, required for '@com'}}
