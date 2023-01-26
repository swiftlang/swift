//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_RUNTIME_H
#define SWIFT_REFLECTION_RUNTIME_H

// OpaqueValue *swift_projectBox(HeapObject *object);
extern void *swift_projectBox(void *object);

// WitnessTable *swift_conformsToProtocol(Metadata *type,
//                                        ProtocolDescriptor *protocol);
extern void *swift_conformsToProtocol(const void *type, const void *protocol);

#endif /* SWIFT_REFLECTION_RUNTIME_H */
