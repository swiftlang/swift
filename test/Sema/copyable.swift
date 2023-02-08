// RUN: %target-typecheck-verify-swift -enable-experimental-move-only

protocol P: _Copyable {}
struct S: P {}

class C: _Copyable {}

@_moveOnly struct MOStruct: _Copyable {} // expected-error {{move-only struct 'MOStruct' cannot conform to '_Copyable'}}
