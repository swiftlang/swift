// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-pch -o %t/sdk-bridging-header.pch %S/Inputs/sdk-bridging-header.h
// RUN: %target-swift-frontend -parse -verify %s -import-objc-header %t/sdk-bridging-header.pch
// REQUIRES: objc_interop

import Foundation

let not = MyPredicate.not()
let and = MyPredicate.and([])
let or = MyPredicate.or([not, and])

