// RUN: %target-swift-frontend -emit-ir -primary-file %s

class Base<T: AnyObject> {}

class Derived: Base<Derived> {}
