// RUN: %target-swift-frontend -typecheck %s -I %S%{fs-sep}Inputs -cxx-interoperability-mode=default -target %target-swift-5.8-abi-triple -dump-requirement-machine 2>&1 | %FileCheck %s

import AnyReferenceFRTs

struct Generic<T> {}
extension Generic where T == RefCountedType {}

// CHECK: τ_0_0.[concrete: RefCountedType] => τ_0_0 [explicit]
// CHECK: τ_0_0.[layout: _AnyReference] => τ_0_0
