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

// RUN: %sourcekitd-test -req=track-compiles == -req=sema %S/Inputs/parse-error-with-sourceLocation.swift -- %S/Inputs/parse-error-with-sourceLocation.swift | %FileCheck %s -check-prefix=PARSE-WITH-SOURCELOCATION
// PARSE-WITH-SOURCELOCATION: key.notification: source.notification.compile-did-finish
// PARSE-WITH-SOURCELOCATION-NEXT: key.diagnostics: [
// PARSE-WITH-SOURCELOCATION-NEXT:   {
// PARSE-WITH-SOURCELOCATION-NEXT:     key.line: 2000
// PARSE-WITH-SOURCELOCATION-NEXT:     key.column: 6
// PARSE-WITH-SOURCELOCATION-NEXT:     key.filepath: "custom.swuft"
// PARSE-WITH-SOURCELOCATION-NEXT:     key.severity: source.diagnostic.severity.error
// PARSE-WITH-SOURCELOCATION-NEXT:     key.description: "function name
// PARSE-WITH-SOURCELOCATION-NEXT:   }
// PARSE-WITH-SOURCELOCATION-NEXT: ]

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

// RUN: %sourcekitd-test -req=track-compiles == -req=sema %s -- %s -Xcc -ivfsoverlay -Xcc /doesnotexist | %FileCheck %s -check-prefix=CLANG_IMPORTER_UNKNOWN
// CLANG_IMPORTER_UNKNOWN: key.notification: source.notification.compile-did-finish,
// CLANG_IMPORTER_UNKNOWN-NEXT: key.diagnostics: [
// CLANG_IMPORTER_UNKNOWN-NEXT:   {
// CLANG_IMPORTER_UNKNOWN-NEXT:     key.filepath: "<unknown>"
// CLANG_IMPORTER_UNKNOWN-NEXT:     key.severity: source.diagnostic.severity.error,
// CLANG_IMPORTER_UNKNOWN-NEXT:     key.offset: 0
// CLANG_IMPORTER_UNKNOWN-NEXT:     key.description: "virtual filesystem{{.*}}not found

// Note: we're missing the "compiler is in code completion mode" diagnostic,
// which is probably just as well.
// RUN: %sourcekitd-test -req=track-compiles == -req=complete -offset=0 %s -- %s | %FileCheck %s -check-prefix=NODIAGS
// RUN: %sourcekitd-test -req=track-compiles == -req=complete -pos=2:1 %S/Inputs/sema-error.swift -- %S/Inputs/sema-error.swift | %FileCheck %s -check-prefix=SEMA

// FIXME: invalid arguments cause us to early-exit and not send the notifications
// RUN_DISABLED: %sourcekitd-test -req=track-compiles == -req=sema %s -- %s -invalid-arg | %FileCheck %s -check-prefix=INVALID_ARG

// RUN: %sourcekitd-test -req=track-compiles == -req=sema %s -- %s -Xcc -invalid-arg | %FileCheck %s -check-prefix=INVALID_ARG_CLANG
// INVALID_ARG_CLANG: key.notification: source.notification.compile-did-finish,
// INVALID_ARG_CLANG-NEXT: key.diagnostics: [
// INVALID_ARG_CLANG-NEXT:   {
// INVALID_ARG_CLANG-NEXT:     key.filepath: "<unknown>"
// INVALID_ARG_CLANG-NEXT:     key.severity: source.diagnostic.severity.error,
// INVALID_ARG_CLANG-NEXT:     key.offset: 0
// INVALID_ARG_CLANG-NEXT:     key.description: "unknown argument

// Ignore the spurious -wmo + -enable-batch-mode warning.
// RUN: %sourcekitd-test -req=track-compiles == -req=sema %s -- %s -enable-batch-mode | %FileCheck %s -check-prefix=NODIAGS
// RUN: %sourcekitd-test -req=track-compiles == -req=complete -offset=0 %s -- %s -enable-batch-mode | %FileCheck %s -check-prefix=NODIAGS
