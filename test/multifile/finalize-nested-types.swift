// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/finalize-nested-types-other.swift -module-name other -emit-module-path %t/other.swiftmodule
// RUN: %target-swift-frontend -typecheck %s -I %t

import other

let t = Foo.self
let n = t.Nested()
