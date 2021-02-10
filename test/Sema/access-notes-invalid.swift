// RUN: %target-swift-frontend -typecheck -primary-file %/s -access-notes-path %/S/Inputs/missing.accessnotes 2>&1 | %FileCheck --check-prefix=CHECK-MISSING %s
// CHECK-MISSING: <unknown>:0: warning: ignored access notes file at 'SOURCE_DIR/test/Sema/Inputs/missing.accessnotes' because it cannot be read: {{[Nn]}}o such file or directory{{$}}

// RUN: %target-typecheck-verify-swift -access-notes-path %S/Inputs/bad.accessnotes -verify-additional-file %S/Inputs/bad.accessnotes

// RUN: %target-typecheck-verify-swift -access-notes-path %S/Inputs/extra.accessnotes -verify-additional-file %S/Inputs/extra.accessnotes

// FIXME: Should diagnose multiple access notes for the same decl

// FIXME: Should diagnose access notes that don't match a decl in the source code
