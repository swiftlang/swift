// Test that time-trace paths from supplementary output file maps are honored.

// RUN: %empty-directory(%t)

// Create supplementary output file map with time-trace entry for this input.
// RUN: echo '"%s": { time-trace: "%t/foo.time-trace.json" }' > %t/filemap.yaml

// RUN: %target-swift-frontend -c %s -o %t/foo.o -supplementary-output-file-map %t/filemap.yaml
// RUN: %FileCheck %s --input-file=%t/foo.time-trace.json

// CHECK: "traceEvents"
