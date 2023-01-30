// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -typecheck -primary-file %t/main.swift -emit-reference-dependencies-path - > %t.swiftdeps

// https://github.com/apple/swift/issues/43875
// https://github.com/apple/swift/issues/43878

protocol Protocol {}
class ConformingClass: Protocol {}
class BaseClass<T: Protocol> {}
class ConcreteClass<T: ConformingClass> : BaseClass<T> {}
