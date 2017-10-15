// RUN: not %target-swift-frontend %s -typecheck

public class A {
  var property: UndeclaredType
  var keyPath: Any {
    return #keyPath(property.foo)
  }
}

public class B {
  var property: UndeclaredType
  var keyPath: Any {
    return [#keyPath(property.foo)]
  }
}
