// RUN: not %target-typecheck-swift %s 2> %t
// RUN: %FileCheck %s < %t

protocol P {}
class C1 {}
struct S {}
class C2 {}

extension C2: S {}
extension C2: C1 {}
extension C2: C1 & P {}
