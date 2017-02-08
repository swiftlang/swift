// RUN: rm -rf %t && mkdir -p %t
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -typecheck -primary-file %t/main.swift -emit-reference-dependencies-path - > %t.swiftdeps

// SR-1267, SR-1270
protocol Protocol {}
class ConformingClass: Protocol {}
class BaseClass<T: Protocol> {}
class ConcreteClass<T: ConformingClass> : BaseClass<T> {}
