// RUN: %target-typecheck-verify-swift
@const let x: Int = 42 // expected-error{{'const' attribute is only valid when experimental feature CompileTimeValues is enabled}}
