// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -cxx-interoperability-mode=default -disable-availability-checking -I %S%{fs-sep}Inputs -module-cache-path %t/mcp -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}methods.h

import Methods

let s1 = HasRenamedInitMethods()
s1.start()
s1.init() // expected-error {{'init' is a member of the type; use 'type(of: ...)' to initialize a new object of the same dynamic type}}
s1.startWith(5)
s1.init(n: 5) 
// expected-error@-1 {{'init' is a member of the type; use 'type(of: ...)' to initialize a new object of the same dynamic type}}
// expected-error@-2 {{incorrect argument label in call (have 'n:', expected 'field:')}}

let _ = HasNonInitializerStaticInitMethod()
let _ = HasNonInitializerStaticInitMethod.nonInitializer(5)
let _ = HasNonInitializerStaticInitMethod(n: 5)
// expected-error@-1 {{argument passed to call that takes no arguments}}

let _ = makeWithWrongFactory(n: 5)
// expected-error@-1 {{cannot find 'makeWithWrongFactory' in scope}}
