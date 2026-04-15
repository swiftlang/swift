// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -O -emit-module-path=%t/inline_dynamic_self_module.swiftmodule -module-name=inline_dynamic_self_module %S/Inputs/inline_dynamic_self_module.swift
// RUN: %target-swift-frontend -I %t -O -emit-ir %s

import inline_dynamic_self_module

class C {
    required init() {}
    static func make() -> Self? {
        withWrapper { _ in self.init() }
    }
}
