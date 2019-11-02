// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: %target-swift-frontend %s -O -whole-module-optimization -emit-sil

class ClassA<T> { }
protocol ProtocolA { }

class MainClass<H> {
    init(x: ClassA<H> & ProtocolA) { }
}

final class ClassB: ClassA<String> { }
extension ClassB: ProtocolA { }

_ = MainClass(x: ClassB())
