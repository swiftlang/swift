extension (Int, Int) {}

// RUN: %sourcekitd-test -req=sema %s -- %s | %FileCheck %s -check-prefix=NO_OVERRIDE

// NO_OVERRIDE:      key.description: "non-nominal type
// NO_OVERRIDE:      key.educational_note_paths: [
// NO_OVERRIDE-NEXT:   share{{[/\\]+}}doc{{[/\\]+}}swift{{[/\\]+}}diagnostics{{[/\\]+}}nominal-types.md"
// NO_OVERRIDE-NEXT: ]

// RUN: %sourcekitd-test -req=sema %s -- -Xfrontend -diagnostic-documentation-path -Xfrontend /educational/notes/path/prefix %s | %FileCheck %s -check-prefix=OVERRIDE

// OVERRIDE:      key.description: "non-nominal type
// OVERRIDE:      key.educational_note_paths: [
// OVERRIDE-NEXT:   "{{[/\\]+}}educational{{[/\\]+}}notes{{[/\\]+}}path{{[/\\]+}}prefix{{[/\\]+}}nominal-types.md"
// OVERRIDE-NEXT: ]

