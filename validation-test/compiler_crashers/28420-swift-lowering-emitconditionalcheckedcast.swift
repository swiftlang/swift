// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not --crash %target-swift-frontend %s -emit-ir
// REQUIRES: asserts
struct A{
func a()->String{
}
}
func b(){
guard let c = A.a as?String else{
}
}
