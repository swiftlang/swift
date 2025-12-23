import SomeModule

let global = 1
struct Parent {
    struct Inner {}
    func method<T>(x: T) where T: Equatable {}
}

extension Parent {
    struct ExtInner {
        func extInnerMethod(z: Inner, other: Array<ExtInner>, again: FromSomeModule) {}
        private struct PrivateType {}
        private func privateFunc(_ x: PrivateType) {}

        @_spi(OtherModule) public struct SPIType {}
        internal func spiFunc(x: SPIType) {}
    }
}

// RUN: %empty-directory(%t)
// RUN: echo 'public struct FromSomeModule {} ' > %t/SomeModule.swift
// RUN: %target-build-swift %t/SomeModule.swift -emit-module -module-name SomeModule -o %t/SomeModule.swiftmodule

// References should cover symbols from the symbol graph declaration fragments, even if not present in the original source.
// RUN:  %sourcekitd-test -req=cursor -pos=3:5 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple -I %t | %FileCheck -check-prefixes=GLOBAL %s
//
// GLOBAL:      SYMBOL GRAPH BEGIN
// GLOBAL:        "declarationFragments": [
// GLOBAL-NEXT:     {
// GLOBAL-NEXT:       "kind": "keyword",
// GLOBAL-NEXT:       "spelling": "let"
// GLOBAL-NEXT:     },
// GLOBAL-NEXT:     {
// GLOBAL-NEXT:       "kind": "text",
// GLOBAL-NEXT:       "spelling": " "
// GLOBAL-NEXT:     },
// GLOBAL-NEXT:     {
// GLOBAL-NEXT:       "kind": "identifier",
// GLOBAL-NEXT:       "spelling": "global"
// GLOBAL-NEXT:     },
// GLOBAL-NEXT:     {
// GLOBAL-NEXT:       "kind": "text",
// GLOBAL-NEXT:       "spelling": ": "
// GLOBAL-NEXT:     },
// GLOBAL-NEXT:     {
// GLOBAL-NEXT:       "kind": "typeIdentifier",
// GLOBAL-NEXT:       "preciseIdentifier": "[[Int_USR:.*]]",
// GLOBAL-NEXT:       "spelling": "Int"
// GLOBAL-NEXT:     }
// GLOBAL-NEXT:   ],
// GLOBAL:      SYMBOL GRAPH END
//
// GLOBAL:      REFERENCED DECLS BEGIN
// GLOBAL-NEXT: [[Int_USR]] | public | <empty> | Swift | System | NonSPI | source.lang.swift
// GLOBAL-NEXT:   Int swift.struct [[Int_USR]]
// GLOBAL-NEXT: REFERENCED DECLS END


// References to unsupported types (like generic parameters) should be ignored.
// RUN:  %sourcekitd-test -req=cursor -pos=6:10 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple -I %t | %FileCheck -check-prefixes=GENERIC %s
//
// GENERIC:      SYMBOL GRAPH BEGIN
// GENERIC:        "declarationFragments": [
// GENERIC-NEXT:     {
// GENERIC-NEXT:       "kind": "keyword",
// GENERIC-NEXT:       "spelling": "func"
// GENERIC-NEXT:     },
// GENERIC-NEXT:     {
// GENERIC-NEXT:       "kind": "text",
// GENERIC-NEXT:       "spelling": " "
// GENERIC-NEXT:     },
// GENERIC-NEXT:     {
// GENERIC-NEXT:       "kind": "identifier",
// GENERIC-NEXT:       "spelling": "method"
// GENERIC-NEXT:     },
// GENERIC-NEXT:     {
// GENERIC-NEXT:       "kind": "text",
// GENERIC-NEXT:       "spelling": "<"
// GENERIC-NEXT:     },
// GENERIC-NEXT:     {
// GENERIC-NEXT:       "kind": "genericParameter",
// GENERIC-NEXT:       "spelling": "T"
// GENERIC-NEXT:     },
// GENERIC-NEXT:     {
// GENERIC-NEXT:       "kind": "text",
// GENERIC-NEXT:       "spelling": ">("
// GENERIC-NEXT:     },
// GENERIC-NEXT:     {
// GENERIC-NEXT:       "kind": "externalParam",
// GENERIC-NEXT:       "spelling": "x"
// GENERIC-NEXT:     },
// GENERIC-NEXT:     {
// GENERIC-NEXT:       "kind": "text",
// GENERIC-NEXT:       "spelling": ": "
// GENERIC-NEXT:     },
// GENERIC-NEXT:     {
// GENERIC-NEXT:       "kind": "typeIdentifier",
// GENERIC-NEXT:       "preciseIdentifier": "s:30cursor_symbol_graph_referenced6ParentV6method1xyx_tSQRzlF1TL_xmfp",
// GENERIC-NEXT:       "spelling": "T"
// GENERIC-NEXT:     },
// GENERIC-NEXT:     {
// GENERIC-NEXT:       "kind": "text",
// GENERIC-NEXT:       "spelling": ") "
// GENERIC-NEXT:     },
// GENERIC-NEXT:     {
// GENERIC-NEXT:       "kind": "keyword",
// GENERIC-NEXT:       "spelling": "where"
// GENERIC-NEXT:     },
// GENERIC-NEXT:     {
// GENERIC-NEXT:       "kind": "text",
// GENERIC-NEXT:       "spelling": " "
// GENERIC-NEXT:     },
// GENERIC-NEXT:     {
// GENERIC-NEXT:       "kind": "typeIdentifier",
// GENERIC-NEXT:       "spelling": "T"
// GENERIC-NEXT:     },
// GENERIC-NEXT:     {
// GENERIC-NEXT:       "kind": "text",
// GENERIC-NEXT:       "spelling": " : "
// GENERIC-NEXT:     },
// GENERIC-NEXT:     {
// GENERIC-NEXT:       "kind": "typeIdentifier",
// GENERIC-NEXT:       "preciseIdentifier": "[[Equatable_USR:.*]]",
// GENERIC-NEXT:       "spelling": "Equatable"
// GENERIC-NEXT:     }
// GENERIC-NEXT:   ],
// GENERIC:      SYMBOL GRAPH END
//
// GENERIC:      REFERENCED DECLS BEGIN
// GENERIC-NEXT: [[Equatable_USR]] | public | <empty> | Swift | System | NonSPI | source.lang.swift
// GENERIC-NEXT:   Equatable swift.protocol [[Equatable_USR]]
// GENERIC-NEXT: REFERENCED DECLS END


// References to unsupported types (like generic parameters) should be ignored.
// RUN: %sourcekitd-test -req=cursor -pos=11:14 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple -I %t | %FileCheck -check-prefixes=NESTED %s
//
// NESTED:      SYMBOL GRAPH BEGIN
// NESTED:        "declarationFragments": [
// NESTED-NEXT:     {
// NESTED-NEXT:       "kind": "keyword",
// NESTED-NEXT:       "spelling": "func"
// NESTED-NEXT:     },
// NESTED-NEXT:     {
// NESTED-NEXT:       "kind": "text",
// NESTED-NEXT:       "spelling": " "
// NESTED-NEXT:     },
// NESTED-NEXT:     {
// NESTED-NEXT:       "kind": "identifier",
// NESTED-NEXT:       "spelling": "extInnerMethod"
// NESTED-NEXT:     },
// NESTED-NEXT:     {
// NESTED-NEXT:       "kind": "text",
// NESTED-NEXT:       "spelling": "("
// NESTED-NEXT:     },
// NESTED-NEXT:     {
// NESTED-NEXT:       "kind": "externalParam",
// NESTED-NEXT:       "spelling": "z"
// NESTED-NEXT:     },
// NESTED-NEXT:     {
// NESTED-NEXT:       "kind": "text",
// NESTED-NEXT:       "spelling": ": "
// NESTED-NEXT:     },
// NESTED-NEXT:     {
// NESTED-NEXT:       "kind": "typeIdentifier",
// NESTED-NEXT:       "preciseIdentifier": "[[Inner_USR:.*]]",
// NESTED-NEXT:       "spelling": "Inner"
// NESTED-NEXT:     },
// NESTED-NEXT:     {
// NESTED-NEXT:       "kind": "text",
// NESTED-NEXT:       "spelling": ", "
// NESTED-NEXT:     },
// NESTED-NEXT:     {
// NESTED-NEXT:       "kind": "externalParam",
// NESTED-NEXT:       "spelling": "other"
// NESTED-NEXT:     },
// NESTED-NEXT:     {
// NESTED-NEXT:       "kind": "text",
// NESTED-NEXT:       "spelling": ": "
// NESTED-NEXT:     },
// NESTED-NEXT:     {
// NESTED-NEXT:       "kind": "typeIdentifier",
// NESTED-NEXT:       "preciseIdentifier": "[[Array_USR:.*]]",
// NESTED-NEXT:       "spelling": "Array"
// NESTED-NEXT:     },
// NESTED-NEXT:     {
// NESTED-NEXT:       "kind": "text",
// NESTED-NEXT:       "spelling": "<"
// NESTED-NEXT:     },
// NESTED-NEXT:     {
// NESTED-NEXT:       "kind": "typeIdentifier",
// NESTED-NEXT:       "preciseIdentifier": "[[ExtInner_USR:.*]]",
// NESTED-NEXT:       "spelling": "ExtInner"
// NESTED-NEXT:     },
// NESTED-NEXT:     {
// NESTED-NEXT:       "kind": "text",
// NESTED-NEXT:       "spelling": ">, "
// NESTED-NEXT:     },
// NESTED-NEXT:     {
// NESTED-NEXT:       "kind": "externalParam",
// NESTED-NEXT:       "spelling": "again"
// NESTED-NEXT:     },
// NESTED-NEXT:     {
// NESTED-NEXT:       "kind": "text",
// NESTED-NEXT:       "spelling": ": "
// NESTED-NEXT:     },
// NESTED-NEXT:     {
// NESTED-NEXT:       "kind": "typeIdentifier",
// NESTED-NEXT:       "preciseIdentifier": "[[FromSomeModule_USR:.*]]",
// NESTED-NEXT:       "spelling": "FromSomeModule"
// NESTED-NEXT:     },
// NESTED-NEXT:     {
// NESTED-NEXT:       "kind": "text",
// NESTED-NEXT:       "spelling": ")"
// NESTED-NEXT:     }
// NESTED-NEXT:   ],
// NESTED:      SYMBOL GRAPH END
//
// NESTED:      REFERENCED DECLS BEGIN
// NESTED-NEXT: [[Inner_USR]] | internal | {{.*}}cursor_symbol_graph_referenced.swift | cursor_symbol_graph_referenced | User | NonSPI | source.lang.swift
// NESTED-NEXT:   Parent swift.struct s:30cursor_symbol_graph_referenced6ParentV
// NESTED-NEXT:   Inner swift.struct [[Inner_USR]]
// NESTED-NEXT: [[Array_USR]] | public | <empty> | Swift | System | NonSPI | source.lang.swift
// NESTED-NEXT:   Array swift.struct [[Array_USR]]
// NESTED-NEXT: [[ExtInner_USR]] | internal | {{.*}}cursor_symbol_graph_referenced.swift | cursor_symbol_graph_referenced | User | NonSPI | source.lang.swift
// NESTED-NEXT:   Parent swift.struct s:30cursor_symbol_graph_referenced6ParentV
// NESTED-NEXT:   ExtInner swift.struct [[ExtInner_USR]]
// NESTED-NEXT: [[FromSomeModule_USR]] | public | {{.*}}SomeModule.swift | SomeModule | User | NonSPI | source.lang.swift
// NESTED-NEXT:   FromSomeModule swift.struct [[FromSomeModule_USR]]
// NESTED-NEXT: REFERENCED DECLS END


// Check access level reporting
// RUN:  %sourcekitd-test -req=cursor -pos=13:22 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple -I %t | %FileCheck -check-prefixes=PRIVATE %s
//
// PRIVATE:      SYMBOL GRAPH BEGIN
// PRIVATE:        "declarationFragments": [
// PRIVATE-NEXT:     {
// PRIVATE-NEXT:       "kind": "keyword",
// PRIVATE-NEXT:       "spelling": "func"
// PRIVATE-NEXT:     },
// PRIVATE-NEXT:     {
// PRIVATE-NEXT:       "kind": "text",
// PRIVATE-NEXT:       "spelling": " "
// PRIVATE-NEXT:     },
// PRIVATE-NEXT:     {
// PRIVATE-NEXT:       "kind": "identifier",
// PRIVATE-NEXT:       "spelling": "privateFunc"
// PRIVATE-NEXT:     },
// PRIVATE-NEXT:     {
// PRIVATE-NEXT:       "kind": "text",
// PRIVATE-NEXT:       "spelling": "("
// PRIVATE-NEXT:     },
// PRIVATE-NEXT:     {
// PRIVATE-NEXT:       "kind": "externalParam",
// PRIVATE-NEXT:       "spelling": "_"
// PRIVATE-NEXT:     },
// PRIVATE-NEXT:     {
// PRIVATE-NEXT:       "kind": "text",
// PRIVATE-NEXT:       "spelling": " "
// PRIVATE-NEXT:     },
// PRIVATE-NEXT:     {
// PRIVATE-NEXT:       "kind": "internalParam",
// PRIVATE-NEXT:       "spelling": "x"
// PRIVATE-NEXT:     },
// PRIVATE-NEXT:     {
// PRIVATE-NEXT:       "kind": "text",
// PRIVATE-NEXT:       "spelling": ": "
// PRIVATE-NEXT:     },
// PRIVATE-NEXT:     {
// PRIVATE-NEXT:       "kind": "typeIdentifier",
// PRIVATE-NEXT:       "preciseIdentifier": "[[PrivateType_USR:.*]]",
// PRIVATE-NEXT:       "spelling": "PrivateType"
// PRIVATE-NEXT:     },
// PRIVATE-NEXT:     {
// PRIVATE-NEXT:       "kind": "text",
// PRIVATE-NEXT:       "spelling": ")"
// PRIVATE-NEXT:     }
// PRIVATE-NEXT:   ],
// PRIVATE:      SYMBOL GRAPH END
//
// PRIVATE:      REFERENCED DECLS BEGIN
// PRIVATE-NEXT: [[PrivateType_USR]] | private | {{.*}}cursor_symbol_graph_referenced.swift | cursor_symbol_graph_referenced | User | NonSPI | source.lang.swift
// PRIVATE-NEXT:   Parent swift.struct s:30cursor_symbol_graph_referenced6ParentV
// PRIVATE-NEXT:   ExtInner swift.struct s:30cursor_symbol_graph_referenced6ParentV8ExtInnerV
// PRIVATE-NEXT:   PrivateType swift.struct [[PrivateType_USR]]
// PRIVATE-NEXT: REFERENCED DECLS END


// Check SPI reporting
// RUN:  %sourcekitd-test -req=cursor -pos=16:23 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple -I %t | %FileCheck -check-prefixes=SPI %s
//
// SPI:      SYMBOL GRAPH BEGIN
// SPI:        "declarationFragments": [
// SPI-NEXT:     {
// SPI-NEXT:       "kind": "keyword",
// SPI-NEXT:       "spelling": "func"
// SPI-NEXT:     },
// SPI-NEXT:     {
// SPI-NEXT:       "kind": "text",
// SPI-NEXT:       "spelling": " "
// SPI-NEXT:     },
// SPI-NEXT:     {
// SPI-NEXT:       "kind": "identifier",
// SPI-NEXT:       "spelling": "spiFunc"
// SPI-NEXT:     },
// SPI-NEXT:     {
// SPI-NEXT:       "kind": "text",
// SPI-NEXT:       "spelling": "("
// SPI-NEXT:     },
// SPI-NEXT:     {
// SPI-NEXT:       "kind": "externalParam",
// SPI-NEXT:       "spelling": "x"
// SPI-NEXT:     },
// SPI-NEXT:     {
// SPI-NEXT:       "kind": "text",
// SPI-NEXT:       "spelling": ": "
// SPI-NEXT:     },
// SPI-NEXT:     {
// SPI-NEXT:       "kind": "typeIdentifier",
// SPI-NEXT:       "preciseIdentifier": "[[SPIType_USR:.*]]",
// SPI-NEXT:       "spelling": "SPIType"
// SPI-NEXT:     },
// SPI-NEXT:     {
// SPI-NEXT:       "kind": "text",
// SPI-NEXT:       "spelling": ")"
// SPI-NEXT:     }
// SPI-NEXT:   ],
// SPI:      SYMBOL GRAPH END
// SPI:      REFERENCED DECLS BEGIN
// SPI-NEXT: [[SPIType_USR]] | public | {{.*}}cursor_symbol_graph_referenced.swift | cursor_symbol_graph_referenced | User | SPI | source.lang.swift
// SPI-NEXT:   Parent swift.struct s:30cursor_symbol_graph_referenced6ParentV
// SPI-NEXT:   ExtInner swift.struct s:30cursor_symbol_graph_referenced6ParentV8ExtInnerV
// SPI-NEXT:   SPIType swift.struct [[SPIType_USR]]
// SPI-NEXT: REFERENCED DECLS END
