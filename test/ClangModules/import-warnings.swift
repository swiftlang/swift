// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify -I %S/Inputs/custom-modules %s
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -I %S/Inputs/custom-modules %s 2>&1 | FileCheck %s

import floats
// CHECK: interface 'long_double_returning_function()' imported from module 'floats' will be unavailable

let v = long_double_returning_function()
// expected-error@-1{{use of unresolved identifier 'long_double_returning_function'}}

