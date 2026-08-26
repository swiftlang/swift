// RUN: %target-swift-emit-silgen-ossa(mock-sdk: %clang-importer-sdk) -o /dev/null -enable-sil-opaque-values -enable-objc-interop -import-objc-header %S/Inputs/objc_bridged_block_optionality_diff.h -verify %s
// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -enable-objc-interop -import-objc-header %S/Inputs/objc_bridged_block_optionality_diff.h -verify %s

// expected-warning@<unknown> * {{libc not found for }}

import Foundation

TheHandlerBlock = { x in () }

