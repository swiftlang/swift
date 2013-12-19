// RUN: rm -rf %t
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%sdk %s -verify
// REQUIRES: sdk

import Foundation

func unavailable_id(a: id) {} // expected-error {{use of undeclared type 'id'}}
func unavailable_Class(a: Class) {} // expected-error {{use of undeclared type 'Class'}}

