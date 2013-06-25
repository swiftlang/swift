// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -o %t/new_module.swiftmodule %S/empty.swift
// RUN: %swift -emit-module -o %t/another_new_module.swiftmodule %S/empty.swift
// RUN: %swift -emit-module -o %t/depends_on_new_module.swiftmodule %S/Inputs/depends_on_new_module.swift -I=%t
// RUN: %swift %s -parse -I=%t
// RUN: rm %t/new_module.swiftmodule
// RUN: %swift %s -parse -I=%t -verify

// This error should happen after we've deleted the dependency module
import depends_on_new_module // expected-error{{missing required module 'new_module'}}

depends_on_new_module // no-warning
