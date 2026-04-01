// RUN: %target-typecheck-verify-swift -cxx-interoperability-mode=default -I %S/Inputs -disable-availability-checking

import POD

struct HasWeakReference {
  weak var x: Empty? // expected-error {{'Empty' is incompatible with 'weak' references, because it is a foreign reference type}}
}

struct HasUnownedReference {
  unowned var x: Empty!
}
