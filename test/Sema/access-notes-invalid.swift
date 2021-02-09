// RUN: %target-swift-frontend -typecheck -primary-file %s -access-notes-path %S/Inputs/missing.accessnotes 2>&1 | %FileCheck --check-prefix=CHECK-MISSING --enable-windows-compatibility %s
// CHECK-MISSING: <unknown>:0: warning: access notes at 'SOURCE_DIR/test/Sema/Inputs/missing.accessnotes' will be ignored due to an error while loading them: No such file or directory{{$}}

// RUN: %target-swift-frontend -typecheck -primary-file %s -access-notes-path %S/Inputs/bad.accessnotes 2>&1 | %FileCheck --check-prefix=CHECK-BAD --enable-windows-compatibility %s
// CHECK-BAD: <unknown>:0: warning: access notes at 'SOURCE_DIR/test/Sema/Inputs/bad.accessnotes' will be ignored due to an error while loading them: not a mapping at line 1, column 0{{$}}

// RUN: %target-swift-frontend -typecheck -primary-file %s -access-notes-path %S/Inputs/extra.accessnotes >%t.txt 2>&1 
// RUN: %FileCheck --check-prefix=CHECK-EXTRA --enable-windows-compatibility %s <%t.txt
// RUN: %FileCheck --check-prefix=NEGATIVE-EXTRA --enable-windows-compatibility %s <%t.txt
// CHECK-EXTRA: <unknown>:0: remark: compiler ignored unknown key 'CorinthianLeather' in access notes at 'SOURCE_DIR/test/Sema/Inputs/extra.accessnotes'
// NEGATIVE-EXTRA-NOT: <unknown>:0: warning: access notes at 'SOURCE_DIR/test/Sema/Inputs/extra.accessnotes' will be ignored due to an error while loading them

// FIXME: Should diagnose multiple access notes for the same decl

// FIXME: Should diagnose access notes that don't match a decl in the source code
