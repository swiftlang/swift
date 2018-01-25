// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen %s -import-objc-header %S/Inputs/objc_bridged_block_optionality_diff.h -verify
import Foundation

TheHandlerBlock = { x in () }

