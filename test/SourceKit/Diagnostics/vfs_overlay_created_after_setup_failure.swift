// A request whose arguments reference a VFS overlay that doesn't exist fails
// with "Setting up virtual file system overlays failed". Once the overlay
// exists, the same request succeeds, because the overlay file is a dependency
// of the AST build even when compiler setup fails.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %sourcekitd-test \
// RUN:   -shell -- echo "### Before creating the overlay" == \
// RUN:   -dont-print-request -allow-error-response -req=diags %t/main.swift -- %t/main.swift -vfsoverlay %t/overlay.yaml == \
// RUN:   -shell -- cp %t/overlay.yaml.in %t/overlay.yaml == \
// RUN:   -shell -- echo "### After creating the overlay" == \
// RUN:   -dont-print-request -req=diags %t/main.swift -- %t/main.swift -vfsoverlay %t/overlay.yaml \
// RUN:   | %FileCheck %s

// CHECK-LABEL: ### Before creating the overlay
// CHECK-NEXT: error response (Request Failed): Setting up virtual file system overlays failed

// CHECK-LABEL: ### After creating the overlay
// CHECK-NEXT: {
// CHECK-NEXT:   key.diagnostics: [
// CHECK-NEXT:   ]
// CHECK-NEXT: }

//--- main.swift
func f() -> Int { return 1 }

//--- overlay.yaml.in
{"version": 0, "roots": []}
