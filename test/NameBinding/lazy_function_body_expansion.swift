// Ensure that a lazily-parsed function body gets expanded

// RUN: %target-swift-frontend -typecheck -primary-file %s %S/Inputs/lazy_function_body_expansion_helper.swift

let a = v
