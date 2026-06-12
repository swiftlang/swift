// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default

import NestedEnums

let _: Struct.Scoped = Struct.Scoped.S1
let _: Struct.Scoped = Struct.Scoped.S2
let _: Struct.Scoped = Struct.Scoped.S3

let _: Struct.Unscoped = Struct.U1
let _: Struct.Unscoped = Struct.U2
let _: Struct.Unscoped = Struct.U3

let _: Int = Struct.A1
let _: Int = Struct.A2
let _: Int = Struct.A3

let _: Namespace.Scoped = Namespace.Scoped.S1
let _: Namespace.Scoped = Namespace.Scoped.S2
let _: Namespace.Scoped = Namespace.Scoped.S3

let _: Namespace.Unscoped = Namespace.U1
let _: Namespace.Unscoped = Namespace.U2
let _: Namespace.Unscoped = Namespace.U3

let _: Int = Namespace.A1
let _: Int = Namespace.A2
let _: Int = Namespace.A3
