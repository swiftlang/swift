// RUN: %empty-directory(%t)
// RUN: echo 'print("Hello, World!")' >%t/main.swift
// RUN: cd %t

// RUN: %target-swift-frontend -emit-module -emit-abi-descriptor-path %t/main.abi.json %t/main.swift
// RUN: test -f %t/main.abi.json

// RUN: echo '"%/t/main.swift": { abi-baseline-json: "%/t/foo.abi.json" }' > %/t/filemap.yaml
// RUN: %target-swift-frontend -emit-module -supplementary-output-file-map %/t/filemap.yaml %/t/main.swift
// RUN: test -f %t/foo.abi.json
