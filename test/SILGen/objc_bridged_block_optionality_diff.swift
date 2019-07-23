// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -enable-objc-interop -import-objc-header %S/Inputs/objc_bridged_block_optionality_diff.h -verify %s

import Foundation

TheHandlerBlock = { x in () }

