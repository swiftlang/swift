// RUN: %target-typecheck-verify-swift
@constInitialized let x: Int = 42 // expected-error{{'constInitialized' attribute is only valid when experimental feature CompileTimeValues is enabled}}
