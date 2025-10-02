// RUN: %target-typecheck-verify-swift -cxx-interoperability-mode=default -module-alias Foo=Bar -verify-ignore-unknown

import Foo // expected-error{{no such module 'Bar'}}
// expected-note@-1{{module name 'Foo' is aliased to 'Bar'}}

