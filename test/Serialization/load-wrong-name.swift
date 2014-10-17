// RUN: rm -rf %t && mkdir %t
// RUN: %swift -emit-module -o %t %S/Inputs/def_func.swift -module-name new_module
// RUN: %swift %s -parse -I %t -show-diagnostics-after-fatal -verify

import swift // expected-error{{cannot load module 'Swift' as 'swift'}}
import NEW_MODULE // expected-error{{cannot load module 'new_module' as 'NEW_MODULE'}}
