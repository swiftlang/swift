// RUN: rm -rf %t && mkdir -p %t
// RUN: cp %s %t/main.swift
// RUN: not %target-swift-frontend -parse -primary-file %t/main.swift -emit-reference-dependencies-path - > %t.swiftdeps

extension Foo {}
