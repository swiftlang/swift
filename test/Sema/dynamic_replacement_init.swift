// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -swift-version 5 -enable-implicit-dynamic -enable-private-imports -emit-module -primary-file %S/Inputs/dynamic_replacements_init_A.swift -emit-module -module-name ModuleA -emit-module-path %t/ModuleA.swiftmodule
// RUN: %target-swift-frontend -swift-version 5 -typecheck -verify %s -I %t

@_private(sourceFile: "dynamic_replacements_init_A.swift") import ModuleA

extension S {
  @_dynamicReplacement(for: init(i:))
  private init(_i: Int) {
    self.i = _i
  }
  @_dynamicReplacement(for: init(y:))
  private init(_y: Int) {
    self.init(i: _y)
  }
}

extension A {
  @_dynamicReplacement(for: init(i:))
  private init(_i: Int) {
    self.i = _i
  }

  @_dynamicReplacement(for: init(c:))
  private convenience init(_y: Int) {
    self.init(i: _y)
  }
}

extension B {
  @_dynamicReplacement(for: init(b:i:))
  private init(_i: Int, _b: Int) {
    self.b = _b
    super.init(i: _i)
  }

  @_dynamicReplacement(for: init(x:))
  private convenience init(_i: Int) {
    self.init(b: _i, i: _i)
  }
}
