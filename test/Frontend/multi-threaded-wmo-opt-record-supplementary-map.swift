// Test that frontend properly handles supplementary output file maps with
// optimization records in multi-threaded WMO mode

// RUN: %empty-directory(%t)

// RUN: echo 'public func funcA() -> Int { return 42 }' > %t/file_a.swift
// RUN: echo 'public func funcB() -> String { return "hello" }' > %t/file_b.swift

// RUN: echo '{' > %t/output-file-map.json
// RUN: echo '  "%/t/file_a.swift": {' >> %t/output-file-map.json
// RUN: echo '    "object": "%/t/file_a.o",' >> %t/output-file-map.json
// RUN: echo '    "yaml-opt-record": "%/t/file_a.opt.yaml",' >> %t/output-file-map.json
// RUN: echo '    "llvm-ir": "%/t/file_a.ll"' >> %t/output-file-map.json
// RUN: echo '  },' >> %t/output-file-map.json
// RUN: echo '  "%/t/file_b.swift": {' >> %t/output-file-map.json
// RUN: echo '    "object": "%/t/file_b.o",' >> %t/output-file-map.json
// RUN: echo '    "yaml-opt-record": "%/t/file_b.opt.yaml",' >> %t/output-file-map.json
// RUN: echo '    "llvm-ir": "%/t/file_b.ll"' >> %t/output-file-map.json
// RUN: echo '  }' >> %t/output-file-map.json
// RUN: echo '}' >> %t/output-file-map.json

// RUN: %target-swift-frontend -c %/t/file_a.swift %/t/file_b.swift \
// RUN:   -wmo -num-threads 2 -O -module-name TestModule \
// RUN:   -supplementary-output-file-map %t/output-file-map.json \
// RUN:   -o %t/file_a.o -o %t/file_b.o

// RUN: ls %t/file_a.o
// RUN: ls %t/file_b.o
// RUN: ls %t/file_a.opt.yaml
// RUN: ls %t/file_b.opt.yaml
// RUN: ls %t/file_a.ll
// RUN: ls %t/file_b.ll

// RUN: grep -q "funcA" %t/file_a.opt.yaml
// RUN: grep -q "funcB" %t/file_b.opt.yaml
// RUN: grep -q "funcA" %t/file_a.ll
// RUN: grep -q "funcB" %t/file_b.ll
