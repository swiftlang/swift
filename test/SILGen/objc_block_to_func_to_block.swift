// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/objc_block_to_func_to_block.h -emit-silgen-ossa -o /dev/null -sil-verify-all -enable-sil-opaque-values -verify %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/objc_block_to_func_to_block.h -emit-silgen -verify %s
// REQUIRES: objc_interop

import Foundation

func bar<A>(x: Foo<A>) {
  x.blockInception { f in f { _ = $0 } }
}

typealias MyBlockWithEscapingParam = (@escaping () -> ()) -> Int

// Make sure that we properly create thunks for objc_block_to_func_to_block
@objc class ObjCClass : NSObject {
  @objc func blockWithBlockTypealias(_ block: MyBlockWithEscapingParam) {}
}
