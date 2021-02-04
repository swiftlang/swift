// RUN: %target-swift-frontend -typecheck -primary-file %s -access-notes-path %S/Inputs/missing.accessnotes 2>&1 | %FileCheck --check-prefix=CHECK-MISSING %s
// CHECK-MISSING: <unknown>:0: warning: access notes at 'SOURCE_DIR/test/Sema/Inputs/missing.accessnotes' will be ignored due to an error while loading them: No such file or directory{{$}}

// RUN: %target-swift-frontend -typecheck -primary-file %s -access-notes-path %S/Inputs/bad.accessnotes 2>&1 | %FileCheck --check-prefix=CHECK-BAD %s
// CHECK-BAD: <unknown>:0: warning: access notes at 'SOURCE_DIR/test/Sema/Inputs/bad.accessnotes' will be ignored due to an error while loading them: not a mapping at line 1, column 0{{$}}
