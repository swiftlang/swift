// RUN: %target-swift-frontend %s -emit-ir

protocol P {}

class Base<T: P> {}

class Derived: Base<Derived>, P {}
