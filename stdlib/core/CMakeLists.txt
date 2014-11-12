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
  BooleanType.swift
  BridgeObjectiveC.swift
  BridgeStorage.swift
  Builtin.swift
  BuiltinMath.swift.gyb
  CString.swift
  CTypes.swift
  Character.swift
  CocoaArray.swift
  Collection.swift
  CompilerProtocols.swift
  Concatenate.swift.gyb
  ContiguousArrayBuffer.swift
  EmptyCollection.swift
  Existential.swift
  Filter.swift.gyb
  FixedPoint.swift.gyb
  FloatingPoint.swift.gyb
  FloatingPointOperations.swift.gyb
  HashedCollections.swift.gyb
  Hashing.swift
  HeapBuffer.swift
  ImplicitlyUnwrappedOptional.swift
  Index.swift
  IntegerArithmetic.swift.gyb
  Join.swift
  Interval.swift.gyb
  LazyCollection.swift.gyb
  LazySequence.swift
  LifetimeManager.swift
  ManagedBuffer.swift
  Map.swift.gyb
  Mirrors.swift.gyb
  Misc.swift
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
  Runtime.swift.gyb
  Sort.swift.gyb
  ShadowProtocols.swift
  Shims.swift
  StaticString.swift
  Stride.swift
  StrideMirrors.swift.gyb
  String.swift
  StringBridge.swift
  StringBuffer.swift
  StringCore.swift
  StringInterpolation.swift.gyb
  StringLegacy.swift
  StringUTF16.swift
  StringUTF8.swift
  StringUTFViewsMirrors.swift.gyb
  StringUnicodeScalarView.swift
  SwiftNativeNSArray.swift
  Unicode.swift
  UnicodeScalar.swift
  UnicodeTrie.swift.gyb
  UnitTestArrayBuffer.swift
  Unmanaged.swift
  UnsafeBufferPointer.swift.gyb
  UnsafePointer.swift.gyb
  )

# The complete list of sources in the core standard library.  Includes
# all the essential sources listed above.
set(SWIFTLIB_SOURCES
  ${SWIFTLIB_ESSENTIAL}
  ### PLEASE KEEP THIS LIST IN ALPHABETICAL ORDER ###
  Availability.swift
  Bit.swift
  CollectionMirrors.swift.gyb
  CollectionOfOne.swift
  Process.swift
  SliceBuffer.swift
  VarArgs.swift
  Zip.swift
  )

if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  set(LINK_FLAGS "-all_load -Xlinker -reexport-lobjc")
  set(RUNTIME_DEPENDENCY swiftRuntime)
else()
  set(LINK_FLAGS)
  # With the GNU linker the equivalent of -all_load is to tell the linker
  # --whole-archive before the archive and --no-whole-archive after (without
  # the second, it causes errors when the system libraries are told to
  # include everything). The best way to get it in there, according to the
  # documentation, is to put the flags in the target_link_Libraries setting.

  # TODO: However, for the moment this actually makes things explode with an
  # incomplete runtime. This should be turned back on when more of the porting
  # effort has been completed.
  #set(LINK_FLAGS
  #  -Wl,--whole-archive swiftRuntime -Wl,--no-whole-archive)
  set(RUNTIME_DEPENDENCY swiftRuntime)
endif()

set(SWIFT_CHECK_ESSENTIAL_STDLIB NO CACHE BOOL
  "Check core standard library layering by linking its essential subset")

if(SWIFT_CHECK_ESSENTIAL_STDLIB)
 add_swift_library(swift_stdlib_essential IS_STDLIB_CORE TARGET_LIBRARY
   ${SWIFTLIB_ESSENTIAL}
   )
  target_link_libraries(swift_stdlib_essential ${RUNTIME_DEPENDENCY})
endif()

set(SHARED_LIBRARY ON)
add_swift_library(swiftCore INSTALL IS_STDLIB_CORE TARGET_LIBRARY
  ${SWIFTLIB_SOURCES}
  # The copy_shim_headers target dependency is required to let the
  # Makefile build know that there's a rule to produce the shims
  # directory, but is not sufficient to cause Swift.o to be rebuilt
  # when the shim header changes.  Therefore, we pass both the target
  # and the generated directory as dependencies
  FILE_DEPENDS
    copy_shim_headers ${SWIFT_LIBRARY_OUTPUT_INTDIR}/swift/shims
  LINK_FLAGS ${LINK_FLAGS}
  DEPENDS ${RUNTIME_DEPENDENCY}
  )
add_dependencies(swiftCore swift)
