// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -c -emit-module-path %t/Foo.swiftmodule -g -cxx-interoperability-mode=default %t/Test.swift -module-cache-path %t/clang-module-cache

//--- Test.swift
// Empty
