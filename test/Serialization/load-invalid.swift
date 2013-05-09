// RUN: %swift %s -parse -verify
// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: touch %t/new_module.sm
// RUN: %swift %s -parse -I=%t -verify

import new_module // expected-error{{no such module 'new_module'}}

new_module // expected-error{{use of unresolved identifier 'new_module'}}
