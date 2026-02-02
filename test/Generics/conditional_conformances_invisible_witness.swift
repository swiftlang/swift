// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/conditional_conformances_invisible_witness_other.swift -emit-module-path %t/conditional_conformances_invisible_witness_other.swiftmodule -verify
// RUN: %target-typecheck-verify-swift -I %t -verify-ignore-unrelated

import conditional_conformances_invisible_witness_other

func takesOptional(_ o: Int?) -> String {
  return o.foo
  // expected-error@-1 {{referencing property 'foo' on 'Optional' requires that 'Int' conform to 'P'}}
}
