#===--- CMakeLists.txt - Reflection support library ---------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2023 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

set(SWIFT_REFLECTION_SWIFT_FLAGS)

list(APPEND SWIFT_REFLECTION_SWIFT_FLAGS
  "-parse-stdlib")

add_swift_target_library(swift_Runtime ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  ContextDescriptor/AnonymousDescriptor.swift
  ContextDescriptor/ClassDescriptor.swift
  ContextDescriptor/ContextDescriptor.swift
  ContextDescriptor/ContextDescriptorValues.swift
  ContextDescriptor/EnumDescriptor.swift
  ContextDescriptor/ExtendedExistentialShape.swift
  ContextDescriptor/ExtensionDescriptor.swift
  ContextDescriptor/FieldDescriptor.swift
  ContextDescriptor/GenericSignature.swift
  ContextDescriptor/ModuleDescriptor.swift
  ContextDescriptor/OpaqueDescriptor.swift
  ContextDescriptor/ProtocolDescriptor.swift
  ContextDescriptor/StructDescriptor.swift
  ContextDescriptor/TypeDescriptor.swift
  ContextDescriptor/TypeReference.swift

  Metadata/ClassMetadata.swift
  Metadata/EnumMetadata.swift
  Metadata/EnumValueWitnessTable.swift
  Metadata/ExistentialMetadata.swift
  Metadata/ExtendedExistentialMetadata.swift
  Metadata/ForeignClassMetadata.swift
  Metadata/ForeignReferenceTypeMetadata.swift
  Metadata/FunctionMetadata.swift
  Metadata/Metadata.swift
  Metadata/MetadataAccessFunction.swift
  Metadata/MetadataValues.swift
  Metadata/MetatypeMetadata.swift
  Metadata/StructMetadata.swift
  Metadata/TupleMetadata.swift
  Metadata/TypeMetadata.swift
  Metadata/ValueWitnessTable.swift

  Utils/BufferView.swift
  Utils/Layouts.swift
  Utils/Lock.swift
  Utils/MangledTypeReference.swift
  Utils/PointerStuff.swift
  Utils/PtrAuth.swift
  Utils/RelativeDirectPointer.swift
  Utils/RelativeDirectPointerIntPair.swift
  Utils/RelativeIndirectablePointer.swift
  Utils/RelativeIndirectablePointerIntPair.swift
  Utils/RelativeIndirectPointer.swift
  Utils/RelativePointer.swift
  Utils/TypeCache.swift

  ConformanceDescriptor.swift
  ExistentialContainer.swift
  Functions.swift
  HeapObject.swift
  WitnessTable.swift

  SWIFT_MODULE_DEPENDS_LINUX Glibc
  SWIFT_MODULE_DEPENDS_FREEBSD Glibc
  SWIFT_MODULE_DEPENDS_OPENBSD Glibc
  SWIFT_MODULE_DEPENDS_CYGWIN Glibc
  SWIFT_MODULE_DEPENDS_HAIKU Glibc
  SWIFT_MODULE_DEPENDS_WINDOWS CRT

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    ${SWIFT_REFLECTION_SWIFT_FLAGS}
  INSTALL_IN_COMPONENT stdlib)
