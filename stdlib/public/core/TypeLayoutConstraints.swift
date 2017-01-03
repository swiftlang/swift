//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Define some compile-known protocols used to constraint type layout.
// These protocols can be used e.g. in @_specialize attribute.

// Type should be a trivial type. It's size should be 8 bits.
public protocol _Trivial8 {
}

// Type should be a trivial type. It's size should be 16 bits.
public protocol _Trivial16 {
}

// Type should be a trivial type. It's size should be 32 bits.
public protocol _Trivial32 {
}

// Type should be a trivial type. It's size should be 64 bits.
public protocol _Trivial64 {
}

// Type should be a trivial type. It's size should be at most 8 bits.
public protocol _TrivialAtMost8 {
}

// Type should be a trivial type. It's size should be at most 16 bits.
public protocol _TrivialAtMost16 {
}

// Type should be a trivial type. It's size should be at most 32 bits.
public protocol _TrivialAtMost32 {
}

// Type should be a trivial type. It's size should be at most 64 bits.
public protocol _TrivialAtMost64 {
}

// Type should be a trivial type.
public protocol _Trivial {
}

// Instances of the type should be reference counter objects.
// For example, classes satisfy this requirement.
public protocol _RefCountedObject {
}

// Instances of the type should be native reference counter objects.
// For example, classes satisfy this requirement.
public protocol _NativeRefCountedObject {
}

