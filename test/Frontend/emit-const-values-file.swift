// RUN: %empty-directory(%t)
// RUN: echo 'print("Hello, World!")' >%t/main.swift
// RUN: cd %t

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/main.swiftconstvalues -primary-file %t/main.swift
// RUN: test -f %t/main.swiftconstvalues

// RUN: echo '"%/t/main.swift": { const-values: "%/t/foo.swiftconstvalues" }' > %/t/filemap.json
// RUN: %target-swift-frontend -typecheck -supplementary-output-file-map %/t/filemap.json -primary-file %/t/main.swift
// RUN: test -f %t/foo.swiftconstvalues

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/main.module.swiftconstvalues %t/main.swift
// RUN: test -f %t/main.module.swiftconstvalues

// RUN: echo '{"%/t/main.swift": { const-values: "%/t/main.module.swiftconstvalues" }}' > %/t/filemap.json
// RUN: %target-swift-frontend -typecheck -supplementary-output-file-map %/t/filemap.json %/t/main.swift
// RUN: test -f %t/main.module.swiftconstvalues
