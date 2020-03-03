extension (Int, Int) {}

// RUN: %sourcekitd-test -req=sema %s -- -Xfrontend -enable-descriptive-diagnostics -Xfrontend -diagnostic-documentation-path -Xfrontend /educational/notes/path/prefix %s | %FileCheck %s -check-prefix=DESCRIPTIVE

// DESCRIPTIVE:      key.description: "non-nominal type
// DESCRIPTIVE:      key.educational_note_paths: [
// DESCRIPTIVE-NEXT:   "{{[/\\]+}}educational{{[/\\]+}}notes{{[/\\]+}}path{{[/\\]+}}prefix{{[/\\]+}}nominal-types.md"
// DESCRIPTIVE-NEXT: ]

// RUN: %sourcekitd-test -req=sema %s -- %s | %FileCheck %s -check-prefix=DESCRIPTIVE-DISABLED

// DESCRIPTIVE-DISABLED:     key.description: "non-nominal type
// DESCRIPTIVE-DISABLED-NOT: key.educational_note_paths
