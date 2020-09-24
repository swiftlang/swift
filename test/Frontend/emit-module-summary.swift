// RUN: %empty-directory(%t)
// RUN: echo 'print("Hello, World!")' >%t/main.swift
// RUN: cd %t

// RUN: %target-swift-frontend -emit-sib -emit-module-summary-path %t/main.swiftmodulesummary %t/main.swift
// RUN: test -f %t/main.swiftmodulesummary

// RUN: echo '"%/t/main.swift": { swiftmodulesummary: "%/t/foo.swiftmodulesummary" }' > %/t/filemap.yaml
// RUN: %target-swift-frontend -emit-sib -supplementary-output-file-map %/t/filemap.yaml %/t/main.swift
// RUN: test -f %t/foo.swiftmodulesummary
