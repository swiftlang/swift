// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
// RUN: %target-swift-frontend -primary-file %s -emit-ir -O
// non-fuzz (@dusek)

func f<T>(_ a:T)->Void{f(nil as[Any]?)}
