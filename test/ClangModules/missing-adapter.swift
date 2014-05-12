// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %swift -emit-module %S/Inputs/adapter.swift -sdk %S/Inputs -I=%S/Inputs/custom-modules -module-name ClangModuleWithAdapter -o %t
// RUN: %swift %s -target x86_64-apple-darwin13 -sdk %S/Inputs -I=%S/Inputs/custom-modules -I %t -module-cache-path %t/clang-module-cache -parse
// RUN: %swift %s -target x86_64-apple-darwin13 -I %t -module-cache-path %t/clang-module-cache -parse -verify

// When run without the underlying SDK, we should get an error here.
import ClangModuleWithAdapter // expected-error{{cannot load underlying module for 'ClangModuleWithAdapter'}}
