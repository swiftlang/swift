// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// These errors are fatal, so test each one separately. Clang modules are used
// because their name lookup is case-sensitive on all platforms, unlike
// serialized Swift modules on case-insensitive filesystems.

// RUN: %target-swift-frontend -typecheck -verify -I %t/include %t/wrong-case.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t/include %t/wrong-case-submodule.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t/include %t/unrelated-name.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t/include -I %t/ambiguous %t/ambiguous-case.swift

//--- include/module.modulemap
module Cheese { }

//--- wrong-case.swift
import cheese // expected-error{{no such module 'cheese'}}
              // expected-note@-1{{did you mean 'Cheese'?}}{{8-14=Cheese}} {{none}}

//--- wrong-case-submodule.swift
import CHEESE.Curds // expected-error{{no such module 'CHEESE.Curds'}}
                    // expected-note@-1{{did you mean 'Cheese'?}}{{8-14=Cheese}} {{none}}

//--- unrelated-name.swift
import Gorgonzola // expected-error{{no such module 'Gorgonzola'}}

//--- ambiguous/module.modulemap
module Fondue { }
module FONDUE { }

//--- ambiguous-case.swift
// No suggestion when several modules match the written name case-insensitively.
import fondue // expected-error{{no such module 'fondue'}}
