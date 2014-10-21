// RUN: rm -rf %t && mkdir %t
// RUN: %swift -emit-module -o %t %S/Inputs/def_func.swift -module-name new_module
// RUN: not %swift %s -parse -I %t -show-diagnostics-after-fatal 2>&1 | FileCheck %s

import swift // CHECK: error: {{cannot load module 'Swift' as 'swift'|no such module 'swift'}}
import NEW_MODULE // CHECK: error: {{cannot load module 'new_module' as 'NEW_MODULE'|no such module 'NEW_MODULE'}}
