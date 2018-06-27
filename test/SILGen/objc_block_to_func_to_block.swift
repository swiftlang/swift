// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/objc_block_to_func_to_block.h -emit-silgen -verify %s
// REQUIRES: objc_interop

import Foundation

func bar<A>(x: Foo<A>) {
  x.blockInception { f in f { _ = $0 } }
}
