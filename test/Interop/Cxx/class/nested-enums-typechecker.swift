// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=default

import NestedEnums

let _: HasEnums.Scoped = HasEnums.Scoped.S1
let _: HasEnums.Scoped = HasEnums.Scoped.S2
let _: HasEnums.Scoped = HasEnums.Scoped.S3

let _: HasEnums.Unscoped = HasEnums.U1
let _: HasEnums.Unscoped = HasEnums.U2
let _: HasEnums.Unscoped = HasEnums.U3

let _: Int = HasEnums.A1
let _: Int = HasEnums.A2
let _: Int = HasEnums.A3
