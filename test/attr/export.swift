// RUN: %target-typecheck-verify-swift

@export(interface)
func f1() { }

@export(implementation)
func f2() { }

@export(nothing) // expected-error{{unknown option 'nothing' for attribute 'export'}}
func f3() { }

@export // expected-error{{expected '(' in 'export' attribute}}
func f4() { }

@export(interface) // expected-error{{'@export(interface)' cannot be used with '@usableFromInline'}}
@usableFromInline
func f5() { }

@export(interface) // expected-error{{'@export(interface)' cannot be used with '@inlinable'}}
@inlinable
func f6() { }

@_neverEmitIntoClient // expected-warning{{'@_neverEmitIntoClient' has been renamed to '@export(interface)'}}{{1-22=@export(interface)}}
func f7() { }
