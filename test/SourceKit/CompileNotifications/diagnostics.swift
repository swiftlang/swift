// RUN: %sourcekitd-test -req=track-compiles == -req=sema %s -- %s | %FileCheck %s -check-prefix=NODIAGS
// NODIAGS: key.notification: source.notification.compile-did-finish
// NODIAGS-NEXT: key.diagnostics: [
// NODIAGS-NEXT: ]

// RUN: %sourcekitd-test -req=track-compiles == -req=sema %S/Inputs/parse-error.swift -- %S/Inputs/parse-error.swift | %FileCheck %s -check-prefix=PARSE
// PARSE: key.notification: source.notification.compile-did-finish
// PARSE-NEXT: key.diagnostics: [
// PARSE-NEXT:   {
// PARSE-NEXT:     key.line: 1
// PARSE-NEXT:     key.column: 6
// PARSE-NEXT:     key.filepath: "{{.*}}parse-error.swift"
// PARSE-NEXT:     key.severity: source.diagnostic.severity.error
// PARSE-NEXT:     key.description: "function name
// PARSE-NEXT:   }
// PARSE-NEXT: ]

// Diagnostic from other file.
// RUN: %sourcekitd-test -req=track-compiles == -req=sema %s -- %s %S/Inputs/parse-error.swift | %FileCheck %s -check-prefix=PARSE

// RUN: %sourcekitd-test -req=track-compiles == -req=sema %S/Inputs/sema-error.swift -- %S/Inputs/sema-error.swift | %FileCheck %s -check-prefix=SEMA
// SEMA: key.notification: source.notification.compile-did-finish
// SEMA-NEXT: key.diagnostics: [
// SEMA-NEXT:   {
// SEMA-NEXT:     key.line: 1
// SEMA-NEXT:     key.column: 5
// SEMA-NEXT:     key.filepath: "{{.*}}sema-error.swift"
// SEMA-NEXT:     key.severity: source.diagnostic.severity.error
// SEMA-NEXT:     key.description: "use of
// SEMA-NEXT:     key.ranges: [

// RUN: %sourcekitd-test -req=track-compiles == -req=sema %s -- %s -Xcc -include -Xcc /doesnotexist | %FileCheck %s -check-prefix=CLANG_IMPORTER
// CLANG_IMPORTER: key.notification: source.notification.compile-did-finish,
// CLANG_IMPORTER-NEXT: key.diagnostics: [
// CLANG_IMPORTER-NEXT:   {
// CLANG_IMPORTER-NEXT:     key.line:
// CLANG_IMPORTER-NEXT:     key.column:
// CLANG_IMPORTER-NEXT:     key.filepath: "<{{.*}}>"
// CLANG_IMPORTER-NEXT:     key.severity: source.diagnostic.severity.error,
// CLANG_IMPORTER-NEXT:     key.description: {{.*}}not found
