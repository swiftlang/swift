// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default \
// RUN:     -verify-additional-file %S/Inputs/protected-special-member.h \
// RUN:     -suppress-notes -verify-ignore-unrelated
//
// UNSUPPORTED: OS=windows-msvc
// Fails on Windows due to https://github.com/swiftlang/swift/issues/67288

import ProtectedSpecialMember

let pd = ProtectedDtor() // expected-error {{cannot find 'ProtectedDtor' in scope}}

let ipd = InheritsProtectedDtor()
let _ = ipd.inDerived
// FIXME: accessing fromBase should be fine, but doesn't work because we rely on
// importing the base class to access its members, but ProtectedDtor is not
// importable because it has a protected destructor.
let _ = ipd.fromBase // expected-error {{value of type 'InheritsProtectedDtor' has no member 'fromBase'}}

let pipd = PrivatelyInheritsProtectedDtor()
let _ = pipd.inDerived

let pdf = ProtectedDtorField() // expected-error {{cannot find 'ProtectedDtorField' in scope}}

let pdbf = ProtectedDtorBaseAndField() // expected-error {{cannot find 'ProtectedDtorBaseAndField' in scope}}

func copy<T: Copyable>(_ _: T) {}

let pc = ProtectedCopy() // expected-error {{cannot find 'ProtectedCopy' in scope}}

let ipc = InheritsProtectedCopy()
copy(ipc)

let pipc = PrivatelyInheritsProtectedCopy()
copy(pipc)

let pipipc = PrivatelyInheritsPrivatelyInheritsProtectedCopy()
copy(pipipc)

let pcf = ProtectedCopyField() // expected-error {{cannot find 'ProtectedCopyField' in scope}}

let pcfb = ProtectedCopyBaseAndField() // expected-error {{cannot find 'ProtectedCopyBaseAndField' in scope}}

let vpc = VecOfProtectedCopy() // expected-error {{cannot find 'VecOfProtectedCopy' in scope}}

let vipc = VecOfInheritsProtectedCopy()
copy(vipc)

let fvpc = FieldVecOfProtectedCopy() // expected-error {{cannot find 'FieldVecOfProtectedCopy' in scope}}

let fvipc = FieldVecOfInheritsProtectedCopy(1, 2, 3)
copy(fvipc)

let pm = ProtectedMove() // expected-error {{cannot find 'ProtectedMove' in scope}}

let ipm = InheritsProtectedMove()
copy(ipm) // expected-error {{conform to 'Copyable'}}

let vpm = VecOfProtectedMove() // expected-error {{cannot find 'VecOfProtectedMove' in scope}}

let vipm = VecOfInheritsProtectedMove()
copy(vipm) // expected-error {{conform to 'Copyable'}}

let fvpm = FieldVecOfProtectedMove() // expected-error {{cannot find 'FieldVecOfProtectedMove' in scope}}

let fvipm = FieldVecOfInheritsProtectedMove()
copy(fvipm) // expected-error {{conform to 'Copyable'}}

let pcwm  = ProtectedCopyWithMove()
copy(pcwm) // expected-error {{conform to 'Copyable'}}

let ipcwm = InheritsProtectedCopyWithMove()
copy(ipcwm)

let pcmf = ProtectedCopyWithMoveField()
copy(pcmf) // expected-error {{conform to 'Copyable'}}

let pcmbf = ProtectedCopyWithMoveBaseAndField()
copy(pcmbf) // expected-error {{conform to 'Copyable'}}
