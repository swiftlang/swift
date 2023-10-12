// RUN: %empty-directory(%t)
// RUN: echo 'print("Hello, World!")' >%t/main.swift
// RUN: cd %t

// RUN: %target-swift-frontend -emit-module -emit-api-descriptor-path %t/main.api.json %t/main.swift
// RUN: test -f %t/main.api.json

// RUN: echo '"%/t/main.swift": { api-descriptor-json: "%/t/foo.api.json" }' > %/t/filemap.yaml
// RUN: %target-swift-frontend -emit-module -supplementary-output-file-map %/t/filemap.yaml %/t/main.swift
// RUN: test -f %t/foo.api.json
