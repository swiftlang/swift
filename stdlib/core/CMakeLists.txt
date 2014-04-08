#===--- CMakeLists.txt - Build the core standard library -----------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

# The list of sources without which it's impossble to build a core
# standard library.  Try to add new standard library sources to
# SWIFTLIB_SOURCES, below, rather than SWIFTLIB_ESSENTIAL, if
# possible, to improve layering.  Check that you got it right by
# configuring with -DSWIFT_CHECK_ESSENTIAL_STDLIB=YES
set(SWIFTLIB_ESSENTIAL
  ### PLEASE KEEP THIS LIST IN ALPHABETICAL ORDER ###
  Algorithm.swift
  Arrays.swift.gyb
  ArrayBuffer.swift
  ArrayBufferType.swift
  ArrayCore.swift
  ArrayType.swift
  AssertCommon.swift
  Bool.swift
  BridgeObjectiveC.swift
  Builtin.swift
  CString.swift
  CTypes.swift
  Character.swift
  Collection.swift
  CompilerProtocols.swift
  ContiguousString.swift
  Dictionary.swift
  EmptyCollection.swift
  Existential.swift
  FixedPoint.swift.gyb
  FloatingPoint.swift.gyb
  FloatingPointOperations.swift
  HeapBuffer.swift
  Index.swift
  IntegerArithmetic.swift.gyb
  LifetimeManager.swift
  LogicValue.swift
  Misc.swift
  Nil.swift
  OpaqueString.swift
  Optional.swift
  OutputStream.swift
  Policy.swift
  REPL.swift
  Range.swift
  Reflection.swift
  StaticString.swift
  String.swift
  StringBridge.swift
  StringBuffer.swift
  StringByte.swift
  StringCore.swift
  StringLegacy.swift
  UTF16Scalars.swift
  UncheckedOptional.swift
  Unicode.swift
  UnicodeScalar.swift
  UnsafeArray.swift
  UnsafePointer.swift
  )

# Use appropriate assertion code
if(SWIFT_ASSERTS)
  list(APPEND SWIFTLIB_ESSENTIAL "AssertDebug.swift")
else()
  list(APPEND SWIFTLIB_ESSENTIAL "Assert.swift")
endif()

# The complete list of sources in the core standard library.  Includes
# all the essential sources listed above.
set(SWIFTLIB_SOURCES
  ${SWIFTLIB_ESSENTIAL}
  ### PLEASE KEEP THIS LIST IN ALPHABETICAL ORDER ###
  Bit.swift
  CollectionOfOne.swift
  InputStream.swift
  Process.swift
  SliceBuffer.swift
  VarArgs.swift
  Zip.swift
  )

set(SWIFT_CHECK_ESSENTIAL_STDLIB NO CACHE BOOL
  "Check core standard library layering by linking its essential subset")

if(SWIFT_CHECK_ESSENTIAL_STDLIB)
 add_swift_library(swift_stdlib_essential IS_STDLIB_CORE
   ${SWIFTLIB_ESSENTIAL}
   DEPENDS swift_runtime)
endif()

set(SHARED_LIBRARY ON)
add_swift_library(swift_stdlib_core INSTALL IS_STDLIB_CORE
  ${SWIFTLIB_SOURCES}
  DEPENDS swift_runtime
  # The copy_shim_headers target dependency is required to let the
  # Makefile build know that there's a rule to produce the shims
  # directory, but is not sufficient to cause Swift.o to be rebuilt
  # when the shim header changes.  Therefore, we pass both the target
  # and the generated directory as dependencies
  FILE_DEPENDS
    copy_shim_headers ${SWIFT_LIBRARY_OUTPUT_INTDIR}/swift/shims
  )
add_dependencies(swift_stdlib_core swift)

add_swift_optimization_flags(swift_stdlib_core)

# Link against Foundation, for ObjC bridging.
set_target_properties(swift_stdlib_core PROPERTIES
                                        LINK_FLAGS "-all_load -Xlinker -reexport-lobjc")
