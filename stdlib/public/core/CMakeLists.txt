#===--- CMakeLists.txt - Build the core standard library -----------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

# The list of sources without which it's impossible to build a core
# standard library.  Try to add new standard library sources to
# SWIFTLIB_SOURCES, below, rather than SWIFTLIB_ESSENTIAL, if
# possible, to improve layering.  Check that you got it right by
# configuring with -DSWIFT_CHECK_ESSENTIAL_STDLIB=YES
set(SWIFTLIB_ESSENTIAL
  ### PLEASE KEEP THIS LIST IN ALPHABETICAL ORDER ###
  # Some files can't be sorted alphabetically, see notes in the list below.
  Algorithm.swift
  AnyUnicode.swift.gyb
  ArrayBody.swift
  ArrayBuffer.swift
  ArrayBufferProtocol.swift
  ArrayCast.swift
  Arrays.swift.gyb
  ArrayType.swift
  Assert.swift
  AssertCommon.swift
  BidirectionalCollection.swift
  Bool.swift
  BoundedBufferReference.swift
  BoundedCapacity.swift
  BridgeObjectiveC.swift
  BridgeStorage.swift
  Builtin.swift
  BuiltinMath.swift.gyb
  Character.swift
  CocoaArray.swift
  Collection.swift
  CollectionAlgorithms.swift.gyb
  Comparable.swift
  CompilerProtocols.swift
  ContiguouslyStored.swift
  ClosedRange.swift
  ContiguousArrayBuffer.swift
  CString.swift
  CTypes.swift
  DebuggerSupport.swift
  DropWhile.swift.gyb
  Dump.swift
  EmptyCollection.swift
  Equatable.swift
  ErrorType.swift
  Existential.swift
  Filter.swift.gyb
  FixedArray.swift
  FixedPoint.swift.gyb
  FlatMap.swift
  Flatten.swift.gyb
  FloatingPoint.swift.gyb
  FloatingPointParsing.swift.gyb
  FloatingPointTypes.swift.gyb
  ForwardingWrapper.swift.gyb
  Hashable.swift
  HashedCollections.swift.gyb
  # WORKAROUND: This file name is not sorted alphabetically in the list because
  # if we do so, the compiler crashes.
  AnyHashable.swift
  # END WORKAROUND
  HashedCollectionsAnyHashableExtensions.swift
  Hashing.swift
  HeapBuffer.swift
  ICU.swift
  ImplicitlyUnwrappedOptional.swift
  Index.swift
  Indices.swift.gyb
  InputStream.swift
  IntegerArithmetic.swift.gyb
  IntegerParsing.swift.gyb
  Integers.swift.gyb
  Join.swift
  LazyCollection.swift.gyb
  LazySequence.swift
  LifetimeManager.swift
  ManagedBuffer.swift
  Map.swift.gyb
  MemoryLayout.swift
  Mirrors.swift.gyb
  Misc.swift
  MutableCollection.swift
  NewtypeWrapper.swift.gyb
  ObjCMirrors.swift
  ObjectIdentifier.swift
  Optional.swift
  OptionSet.swift
  OutputStream.swift
  Pointer.swift
  Policy.swift
  PrefixWhile.swift.gyb
  Print.swift
  RandomAccessCollection.swift
  Range.swift.gyb
  RangeReplaceableCollection.swift.gyb
  ReflectionLegacy.swift
  Repeat.swift
  REPL.swift
  Reverse.swift
  Runtime.swift.gyb
  SipHash.swift.gyb
  Sequence.swift
  SequenceAlgorithms.swift.gyb
  SequenceWrapper.swift
  SetAlgebra.swift
  ShadowProtocols.swift
  Shims.swift
  Slice.swift.gyb
  Sort.swift.gyb
  StaticString.swift
  Stride.swift.gyb
  StringCharacterView.swift # ORDER DEPENDENCY: Must precede String.swift
  String.swift
  StringBridge.swift
  StringBuffer.swift
  StringComparable.swift
  StringCore.swift
  StringHashable.swift
  StringInterpolation.swift
  StringLegacy.swift
  StringRangeReplaceableCollection.swift.gyb
  StringIndexConversions.swift
  StringStorage.swift
  StringUnicodeScalarView.swift
  StringUTF16.swift
  StringUTF8.swift
  SwiftNativeNSArray.swift
  TranscodedView.swift
  UnavailableStringAPIs.swift.gyb
  UnboundCapacity.swift
  Unicode.swift
  Unicode2.swift
  UnicodeScalar.swift
  UnicodeViews.swift
  UnicodeTrie.swift.gyb
  Unmanaged.swift
  UnsafeBitMap.swift
  UnsafeBufferPointer.swift.gyb
  UnsafeRawBufferPointer.swift.gyb
  UnsafePointer.swift.gyb
  UnsafeRawPointer.swift.gyb
  WriteBackMutableSlice.swift
  )

# The complete list of sources in the core standard library.  Includes
# all the essential sources listed above.
set(SWIFTLIB_SOURCES
  ${SWIFTLIB_ESSENTIAL}
  ### PLEASE KEEP THIS LIST IN ALPHABETICAL ORDER ###
  Availability.swift
  CollectionOfOne.swift
  ExistentialCollection.swift.gyb
  Mirror.swift
  CommandLine.swift
  SliceBuffer.swift
  Tuple.swift.gyb
  UnfoldSequence.swift
  VarArgs.swift
  Zip.swift
  )
set(GROUP_INFO_JSON_FILE ${CMAKE_CURRENT_SOURCE_DIR}/GroupInfo.json)
set(swift_core_link_flags "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}")
set(swift_core_framework_depends)
set(swift_core_private_link_libraries)
set(swift_stdlib_compile_flags "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}")
if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  list(APPEND swift_core_link_flags "-all_load")
  list(APPEND swift_core_framework_depends Foundation)
  list(APPEND swift_core_framework_depends CoreFoundation)
  list(APPEND swift_core_private_link_libraries -licucore)
else()
  # With the GNU linker the equivalent of -all_load is to tell the linker
  # --whole-archive before the archive and --no-whole-archive after (without
  # the second, it causes errors when the system libraries are told to
  # include everything). The best way to get it in there, according to the
  # documentation, is to put the flags in the target_link_libraries setting.

  # TODO: However, for the moment this actually makes things explode with an
  # incomplete runtime. This should be turned back on when more of the porting
  # effort has been completed.
  #set(LINK_FLAGS
  #  -Wl,--whole-archive swiftRuntime -Wl,--no-whole-archive)
  if("${SWIFT_PATH_TO_LIBICU_BUILD}" STREQUAL "")
    list(APPEND swift_core_private_link_libraries
      ICU_UC ICU_I18N)
  else()
    list(APPEND swift_core_private_link_libraries -licui18n -licuuc -licudata)
  endif()
endif()

if("${CMAKE_SYSTEM_NAME}" STREQUAL "CYGWIN")
  execute_process(COMMAND "cygpath" "-u" "$ENV{SYSTEMROOT}"
      OUTPUT_VARIABLE ENV_SYSTEMROOT)

  list(APPEND swift_core_link_flags "${ENV_SYSTEMROOT}/system32/psapi.dll")
endif()

if(CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
  find_library(EXECINFO_LIBRARY execinfo)
  list(APPEND swift_core_private_link_libraries
      ${EXECINFO_LIBRARY})
endif()

option(SWIFT_CHECK_ESSENTIAL_STDLIB
    "Check core standard library layering by linking its essential subset"
    FALSE)

if(SWIFT_STDLIB_SIL_DEBUGGING)
  list(APPEND swift_stdlib_compile_flags "-Xfrontend" "-gsil")
endif()

if(SWIFT_CHECK_ESSENTIAL_STDLIB)
  add_swift_library(swift_stdlib_essential ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB IS_STDLIB_CORE
      ${SWIFTLIB_ESSENTIAL})
  target_link_libraries(swift_stdlib_essential ${RUNTIME_DEPENDENCY})
endif()


set(shared_only_libs)
if(SWIFT_BUILD_STATIC_STDLIB AND "${SWIFT_HOST_VARIANT_SDK}" STREQUAL "LINUX")
   list(APPEND shared_only_libs swiftImageInspectionShared)
endif()

add_swift_library(swiftCore ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB IS_STDLIB_CORE
  ${SWIFTLIB_SOURCES}
  # The copy_shim_headers target dependency is required to let the
  # build system know that there's a rule to produce the shims
  # directory, but is not sufficient to cause the object file to be rebuilt
  # when the shim header changes.  Therefore, we pass both the target
  # and the generated directory as dependencies.
  FILE_DEPENDS
    copy_shim_headers "${SWIFTLIB_DIR}/shims"
    section_magic
    ${GROUP_INFO_JSON_FILE}
  SWIFT_COMPILE_FLAGS ${swift_stdlib_compile_flags}
  LINK_FLAGS ${swift_core_link_flags}
  PRIVATE_LINK_LIBRARIES ${swift_core_private_link_libraries}
  INCORPORATE_OBJECT_LIBRARIES swiftRuntime swiftStdlibStubs
  INCORPORATE_OBJECT_LIBRARIES_SHARED_ONLY ${shared_only_libs}
  FRAMEWORK_DEPENDS ${swift_core_framework_depends}
  INSTALL_IN_COMPONENT stdlib)
