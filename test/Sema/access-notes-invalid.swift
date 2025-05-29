// RUN: %target-swift-frontend -typecheck -primary-file %/s -access-notes-path %/S/Inputs/missing.accessnotes 2>&1 | %FileCheck --check-prefix=CHECK-MISSING %s
// CHECK-MISSING: <unknown>:0: warning: ignored access notes file at 'SOURCE_DIR/test/Sema/Inputs/missing.accessnotes' because it cannot be read: {{[Nn]}}o such file or directory{{$}}

// RUN: %target-typecheck-verify-swift -access-notes-path %S/Inputs/bad.accessnotes -verify-additional-file %S/Inputs/bad.accessnotes

// RUN: %target-typecheck-verify-swift -access-notes-path %S/Inputs/extra.accessnotes -verify-additional-file %S/Inputs/extra.accessnotes

// RUN: %target-swift-frontend -typecheck -primary-file %/s -access-notes-path %/S/Inputs/extra.accessnotes -Raccess-note=none 2>&1 | %FileCheck --check-prefixes=GOOD-IGNORE,BAD-IGNORE %s
// RUN: %target-swift-frontend -typecheck -primary-file %/s -access-notes-path %/S/Inputs/extra.accessnotes -Raccess-note=failures 2>&1 | %FileCheck --check-prefixes=GOOD-IGNORE,BAD-REMARK %s
// RUN: %target-swift-frontend -typecheck -primary-file %/s -access-notes-path %/S/Inputs/extra.accessnotes -Raccess-note=all 2>&1 | %FileCheck --check-prefixes=GOOD-REMARK,BAD-REMARK %s
// RUN: not %target-swift-frontend -typecheck -primary-file %/s -access-notes-path %/S/Inputs/extra.accessnotes -Raccess-note=all-validate 2>&1 | %FileCheck --check-prefixes=GOOD-REMARK,BAD-ERROR %s

// Default should be 'all'.
// RUN: %target-swift-frontend -typecheck -primary-file %/s -access-notes-path %/S/Inputs/extra.accessnotes 2>&1 | %FileCheck --check-prefixes=GOOD-REMARK,BAD-REMARK %s

class Extant {
  func good(_: Int) {} // expected-remark * {{}} expected-note * {{}}
  // GOOD-IGNORE-NOT: access-notes-invalid.swift:[[@LINE-1]]:{{[0-9]+}}: remark: implicitly added '@objc' to this instance method, as specified by access note for Access notes containing future, unknown syntax
  // GOOD-IGNORE-NOT: access-notes-invalid.swift:[[@LINE-2]]:{{[0-9]+}}: note: add '@objc' explicitly to silence this warning
  // GOOD-REMARK-DAG: access-notes-invalid.swift:[[@LINE-3]]:{{[0-9]+}}: remark: implicitly added '@objc' to this instance method, as specified by access note for Access notes containing future, unknown syntax
  // GOOD-REMARK-DAG: note: add '@objc' explicitly to silence this warning

  func bad(_: Int?) {} // expected-remark * {{}}
  // BAD-IGNORE-NOT: access-notes-invalid.swift:[[@LINE-1]]:{{[0-9]+}}: remark: ignored access note: instance method cannot be marked '@objc' by an access note because the type of the parameter cannot be represented in Objective-C; did not implicitly add '@objc' to this instance method, even though it was specified by access note for Access notes containing future, unknown syntax
  // BAD-IGNORE-NOT: access-notes-invalid.swift:[[@LINE-2]]:{{[0-9]+}}: error: ignored access note: instance method cannot be marked '@objc' by an access note because the type of the parameter cannot be represented in Objective-C; did not implicitly add '@objc' to this instance method, even though it was specified by access note for Access notes containing future, unknown syntax
  // BAD-REMARK-DAG: access-notes-invalid.swift:[[@LINE-3]]:{{[0-9]+}}: remark: ignored access note: instance method cannot be marked '@objc' by an access note because the type of the parameter cannot be represented in Objective-C; did not implicitly add '@objc' to this instance method, even though it was specified by access note for Access notes containing future, unknown syntax
  // BAD-ERROR-DAG: access-notes-invalid.swift:[[@LINE-4]]:{{[0-9]+}}: error: ignored access note: instance method cannot be marked '@objc' by an access note because the type of the parameter cannot be represented in Objective-C; did not implicitly add '@objc' to this instance method, even though it was specified by access note for Access notes containing future, unknown syntax
}

// FIXME: Should diagnose multiple access notes for the same decl

// FIXME: Should diagnose access notes that don't match a decl in the source code

// Only valid on platforms where @objc is valid.
// REQUIRES: objc_interop
