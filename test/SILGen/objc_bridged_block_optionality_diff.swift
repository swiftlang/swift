// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) %s -import-objc-header %S/Inputs/objc_bridged_block_optionality_diff.h -verify
// REQUIRES: objc_interop
import Foundation

TheHandlerBlock = { x in () }

