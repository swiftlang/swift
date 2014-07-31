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
  ArrayBody.swift
  ArrayBuffer.swift
  ArrayBufferType.swift
  ArrayCast.swift
  ArrayType.swift
  Arrays.swift.gyb
  Assert.swift
  AssertCommon.swift
  Bool.swift
  BridgeObjectiveC.swift
  Builtin.swift
  BuiltinMath.swift.gyb
  CString.swift
  CTypes.swift
  Character.swift
  CocoaArray.swift
  Collection.swift
  CompilerProtocols.swift
  ContiguousArrayBuffer.swift
  Dictionary.swift
  EmptyCollection.swift
  Existential.swift
  Filter.swift.gyb
  FixedPoint.swift.gyb
  FloatingPoint.swift.gyb
  FloatingPointOperations.swift.gyb
  HeapBuffer.swift
  ImplicitlyUnwrappedOptional.swift
  Index.swift
  IntegerArithmetic.swift.gyb
  Join.swift
  Interval.swift.gyb
  LazyCollection.swift.gyb
  LazySequence.swift
  LifetimeManager.swift
  LogicValue.swift
  Map.swift.gyb
  Mirrors.swift.gyb
  Misc.swift
  NSSwiftArray.swift
  Nil.swift
  Optional.swift
  OutputStream.swift
  Pointer.swift
  Policy.swift
  REPL.swift
  Range.swift
  RangeMirrors.swift.gyb
  RangeReplaceableCollectionType.swift
  Reflection.swift
  Repeat.swift
  Reverse.swift.gyb
  Shims.swift
  StaticString.swift
  Stride.swift
  String.swift
  StringBridge.swift
  StringBuffer.swift
  StringCore.swift
  StringLegacy.swift
  StringUTF16.swift
  StringUTF8.swift
  StringUTFViewsMirrors.swift.gyb
  StringUnicodeScalarView.swift
  Unicode.swift
  UnicodeScalar.swift
  UnicodeTrie.swift.gyb
  Unmanaged.swift
  UnsafeBufferPointer.swift.gyb
  UnsafePointer.swift.gyb
  )

# The complete list of sources in the core standard library.  Includes
# all the essential sources listed above.
set(SWIFTLIB_SOURCES
  ${SWIFTLIB_ESSENTIAL}
  ### PLEASE KEEP THIS LIST IN ALPHABETICAL ORDER ###
  Bit.swift
  CollectionMirrors.swift.gyb
  CollectionOfOne.swift
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
   DEPENDS swiftRuntime)
endif()

set(SHARED_LIBRARY ON)
add_swift_library(swiftCore INSTALL IS_STDLIB_CORE TARGET_LIBRARY
  ${SWIFTLIB_SOURCES}
  DEPENDS swiftRuntime
  # The copy_shim_headers target dependency is required to let the
  # Makefile build know that there's a rule to produce the shims
  # directory, but is not sufficient to cause Swift.o to be rebuilt
  # when the shim header changes.  Therefore, we pass both the target
  # and the generated directory as dependencies
  FILE_DEPENDS
    copy_shim_headers ${SWIFT_LIBRARY_OUTPUT_INTDIR}/swift/shims
  LINK_FLAGS "-all_load -Xlinker -reexport-lobjc"
  )
add_dependencies(swiftCore swift)
