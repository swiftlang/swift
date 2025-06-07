// RUN: %target-typecheck-verify-swift -enable-batch-mode %S/Inputs/fixit_stub_batch_mode_helper.swift

extension C: P {} // expected-error{{type 'C' does not conform to protocol 'P'}} expected-note {{add stubs for conformance}}
