extension (Int, Int) {}

// RUN: %sourcekitd-test -req=sema %s -- -Xfrontend -print-educational-notes -Xfrontend -diagnostic-documentation-path -Xfrontend /educational/notes/path/prefix %s | %FileCheck %s -check-prefix=DESCRIPTIVE

// DESCRIPTIVE:      key.description: "non-nominal type
// DESCRIPTIVE:      key.educational_note_paths: [
// DESCRIPTIVE-NEXT:   "{{[/\\]+}}educational{{[/\\]+}}notes{{[/\\]+}}path{{[/\\]+}}prefix{{[/\\]+}}nominal-types.md"
// DESCRIPTIVE-NEXT: ]
