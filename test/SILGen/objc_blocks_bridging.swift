// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -verify -emit-silgen -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/Inputs -I %S/Inputs -enable-source-import -enable-block-bridging %s | FileCheck %s

import Foundation

@objc class Foo {
  // CHECK-LABEL: sil @_TToFCSo3Foo3foofS_FT1fFSiSi1xSi_Si : $@cc(objc_method) @thin (@cc(cdecl) @objc_block (Int) -> Int, Int, Foo) -> Int 
  // TODO: copy_block
  // CHECK:         [[NATIVE:%.*]] = function_ref @_TFCSo3Foo3foofS_FT1fFSiSi1xSi_Si : $@cc(method) @thin (@owned @callee_owned (Int) -> Int, Int, @owned Foo) -> Int
  // TODO: apply bridging thunk
  // CHECK:         apply [[NATIVE]](undef, %1, %2)
  @objc func foo(f: Int -> Int, x: Int) -> Int { // expected-error{{not implemented}}
    return f(x)
  }

  // CHECK-LABEL: sil @_TToFCSo3Foo3barfS_FT1fFSSSS1xSS_SS : $@cc(objc_method) @thin (@cc(cdecl) @objc_block (NSString) -> @autoreleased NSString, NSString, Foo) -> @autoreleased NSString 
  // TODO: copy_block
  // CHECK:         [[NATIVE:%.*]] = function_ref @_TFCSo3Foo3barfS_FT1fFSSSS1xSS_SS : $@cc(method) @thin (@owned @callee_owned (@owned String) -> @owned String, @owned String, @owned Foo) -> @owned String
  // TODO: apply bridging thunk
  // CHECK:         apply [[NATIVE]](undef, {{%.*}}, %2)
  @objc func bar(f: String -> String, x: String) -> String { // expected-error{{not implemented}}
    return f(x)
  }

  // CHECK-LABEL: sil @_TToFCSo3Foo3basfS_FT1fFGSqSS_GSqSS_1xGSqSS__GSqSS_ : $@cc(objc_method) @thin (@cc(cdecl) @objc_block (Optional<NSString>) -> @autoreleased Optional<NSString>, Optional<NSString>, Foo) -> @autoreleased Optional<NSString> {
  // TODO: copy_block
  // CHECK:         [[NATIVE:%.*]] = function_ref @_TFCSo3Foo3basfS_FT1fFGSqSS_GSqSS_1xGSqSS__GSqSS_ : $@cc(method) @thin (@owned @callee_owned (@owned Optional<String>) -> @owned Optional<String>, @owned Optional<String>, @owned Foo) -> @owned Optional<String>
  // CHECK:         apply [[NATIVE]](undef, {{%.*}}, %2)
  @objc func bas(f: String? -> String?, x: String?) -> String? { // expected-error{{not implemented}}
    return f(x)
  }

  // TODO: Optional functions. We need to suppress reabstraction of the block.
  /*
  @objc func optFunc(f: (String -> String)?, x: String) -> String? {
    return f?(x)
  }
   */
}


