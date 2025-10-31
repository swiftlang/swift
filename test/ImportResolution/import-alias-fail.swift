// RUN: %target-typecheck-verify-swift -module-alias Foo=Bar

import Foo // expected-error{{no such module 'Bar'}}
// expected-note@-1{{module name 'Foo' is aliased to 'Bar'}}

