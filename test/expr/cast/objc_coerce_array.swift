// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -verify %s
// REQUIRES: objc_interop
import Foundation

var x = 1

_ = [x] as [NSNumber]
