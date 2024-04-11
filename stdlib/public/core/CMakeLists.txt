#===--- CMakeLists.txt - Build the core standard library -----------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

function(split_embedded_sources)
  cmake_parse_arguments(SPLIT1 "" "OUT_LIST_EMBEDDED;OUT_LIST_NORMAL" "EMBEDDED;NORMAL" ${ARGN})
  string(REPLACE "EMBEDDED" "NORMAL" ARGN2 "${ARGN}")
  cmake_parse_arguments(SPLIT2 "" "OUT_LIST_EMBEDDED;OUT_LIST_NORMAL" "EMBEDDED;NORMAL" ${ARGN2})
  set(${SPLIT1_OUT_LIST_EMBEDDED} "${SPLIT1_EMBEDDED}" PARENT_SCOPE)
  set(${SPLIT1_OUT_LIST_NORMAL} "${SPLIT2_NORMAL}" PARENT_SCOPE)
endfunction()

# The complete list of sources in the core standard library. Each file is
# annotated with either "EMBEDDED" which means it contributes to both the
# embedded Swift stdlib and the regular stdlib, or "NORMAL" which means it only
# contributes to the regular stdlib. The split_embedded_sources splits this list
# into SWIFTLIB_EMBEDDED_SOURCES and SWIFTLIB_SOURCES accordingly.
split_embedded_sources(
  OUT_LIST_EMBEDDED SWIFTLIB_EMBEDDED_SOURCES
  OUT_LIST_NORMAL SWIFTLIB_SOURCES

  ### -- PLEASE KEEP THIS LIST IN ALPHABETICAL ORDER ###
  # Some files can't be sorted alphabetically, see notes in the list below.

### Array
  EMBEDDED Array/Array.swift
  EMBEDDED Array/ArrayBody.swift
  EMBEDDED Array/ArrayBuffer.swift
  EMBEDDED Array/ArrayBufferProtocol.swift
    NORMAL Array/ArrayCast.swift
  EMBEDDED Array/ArrayShared.swift
  EMBEDDED Array/ArraySlice.swift
  EMBEDDED Array/ArrayType.swift
    NORMAL Array/CocoaArray.swift
  EMBEDDED Array/ContiguousArray.swift
  EMBEDDED Array/ContiguousArrayBuffer.swift
    NORMAL Array/ContiguouslyStored.swift
    NORMAL Array/FixedArray.swift
  EMBEDDED Array/SwiftNativeNSArray.swift

### Bridging
    NORMAL Bridging/BridgeObjectiveC.swift
  EMBEDDED Bridging/BridgeStorage.swift
    NORMAL Bridging/BridgingBuffer.swift
    NORMAL Bridging/NewtypeWrapper.swift

### Floats
  EMBEDDED Floats/FloatingPoint.swift
  EMBEDDED Floats/FloatingPointRandom.swift

### HashedCollections
  EMBEDDED HashedCollections/AnyHashable.swift
  EMBEDDED HashedCollections/Bitset.swift
  EMBEDDED HashedCollections/Dictionary.swift
  EMBEDDED HashedCollections/DictionaryBridging.swift
  EMBEDDED HashedCollections/DictionaryBuilder.swift
  EMBEDDED HashedCollections/DictionaryCasting.swift
  EMBEDDED HashedCollections/DictionaryStorage.swift
  EMBEDDED HashedCollections/DictionaryVariant.swift
  EMBEDDED HashedCollections/Hasher.swift
    NORMAL HashedCollections/Hashing.swift
  EMBEDDED HashedCollections/HashTable.swift
  EMBEDDED HashedCollections/NativeDictionary.swift
  EMBEDDED HashedCollections/NativeSet.swift
  EMBEDDED HashedCollections/Set.swift
    NORMAL HashedCollections/SetAnyHashableExtensions.swift
    NORMAL HashedCollections/SetBridging.swift
  EMBEDDED HashedCollections/SetBuilder.swift
  EMBEDDED HashedCollections/SetCasting.swift
  EMBEDDED HashedCollections/SetStorage.swift
  EMBEDDED HashedCollections/SetVariant.swift
  EMBEDDED HashedCollections/SipHash.swift

### Integers
  EMBEDDED Integers/BuiltinMath.swift
  EMBEDDED Integers/Int128.swift
    NORMAL Integers/IntegerParsing.swift
  EMBEDDED Integers/Integers.swift
    NORMAL Integers/StaticBigInt.swift
  EMBEDDED Integers/UInt128.swift

### Protocols
    NORMAL Protocols/Codable.swift
  EMBEDDED Protocols/Comparable.swift
  EMBEDDED Protocols/CompilerProtocols.swift
  EMBEDDED Protocols/Equatable.swift
  EMBEDDED Protocols/ErrorType.swift
  EMBEDDED Protocols/Hashable.swift
  EMBEDDED Protocols/Identifiable.swift
  EMBEDDED Protocols/OptionSet.swift
  EMBEDDED Protocols/Sendable.swift
  EMBEDDED Protocols/SetAlgebra.swift
  EMBEDDED Protocols/ShadowProtocols.swift

### Reflection
    NORMAL Reflection/DebuggerSupport.swift
    NORMAL Reflection/Dump.swift
    NORMAL Reflection/Mirror.swift
    NORMAL Reflection/Mirrors.swift
  EMBEDDED Reflection/ObjectIdentifier.swift
    NORMAL Reflection/ReflectionMirror.swift

### Sequences+Collections
  EMBEDDED Sequences+Collections/Algorithm.swift
  EMBEDDED Sequences+Collections/BidirectionalCollection.swift
  EMBEDDED Sequences+Collections/ClosedRange.swift
  EMBEDDED Sequences+Collections/Collection.swift
  EMBEDDED Sequences+Collections/CollectionAlgorithms.swift
    NORMAL Sequences+Collections/CollectionDifference.swift
  EMBEDDED Sequences+Collections/CollectionOfOne.swift
    NORMAL Sequences+Collections/Diffing.swift
    NORMAL Sequences+Collections/DiscontiguousSlice.swift
    NORMAL Sequences+Collections/DropWhile.swift
  EMBEDDED Sequences+Collections/EmptyCollection.swift
  EMBEDDED Sequences+Collections/ExistentialCollection.swift
    NORMAL Sequences+Collections/Filter.swift
    NORMAL Sequences+Collections/FlatMap.swift
    NORMAL Sequences+Collections/Flatten.swift
  EMBEDDED Sequences+Collections/Indices.swift
    NORMAL Sequences+Collections/Join.swift
    NORMAL Sequences+Collections/KeyValuePairs.swift
  EMBEDDED Sequences+Collections/LazyCollection.swift
  EMBEDDED Sequences+Collections/LazySequence.swift
    NORMAL Sequences+Collections/Map.swift
  EMBEDDED Sequences+Collections/MutableCollection.swift
    NORMAL Sequences+Collections/PrefixWhile.swift
  EMBEDDED Sequences+Collections/RandomAccessCollection.swift
  EMBEDDED Sequences+Collections/Range.swift
  EMBEDDED Sequences+Collections/RangeReplaceableCollection.swift
    NORMAL Sequences+Collections/RangeSet.swift
    NORMAL Sequences+Collections/RangeSetRanges.swift
  EMBEDDED Sequences+Collections/Repeat.swift
  EMBEDDED Sequences+Collections/Reverse.swift
  EMBEDDED Sequences+Collections/Sequence.swift
  EMBEDDED Sequences+Collections/SequenceAlgorithms.swift
  EMBEDDED Sequences+Collections/Slice.swift
  EMBEDDED Sequences+Collections/SliceBuffer.swift
  EMBEDDED Sequences+Collections/Sort.swift
  EMBEDDED Sequences+Collections/Stride.swift
    NORMAL Sequences+Collections/UIntBuffer.swift
    NORMAL Sequences+Collections/UnfoldSequence.swift
  EMBEDDED Sequences+Collections/WriteBackMutableSlice.swift
  EMBEDDED Sequences+Collections/Zip.swift

### String
    NORMAL String/Character.swift
    NORMAL String/CharacterProperties.swift
    NORMAL String/CString.swift
    NORMAL String/SmallString.swift
  EMBEDDED String/StaticString.swift
    NORMAL String/String.swift
    NORMAL String/StringBreadcrumbs.swift
    NORMAL String/StringBridge.swift
    NORMAL String/StringCharacterView.swift
    NORMAL String/StringComparable.swift
    NORMAL String/StringComparison.swift
    NORMAL String/StringCreate.swift
    NORMAL String/StringGraphemeBreaking.swift
    NORMAL String/StringGuts.swift
    NORMAL String/StringGutsRangeReplaceable.swift
    NORMAL String/StringGutsSlice.swift
    NORMAL String/StringHashable.swift
    NORMAL String/StringIndex.swift
    NORMAL String/StringIndexConversions.swift
    NORMAL String/StringIndexValidation.swift
    NORMAL String/StringInterpolation.swift
    NORMAL String/StringLegacy.swift
    NORMAL String/StringNormalization.swift
    NORMAL String/StringObject.swift
    NORMAL String/StringProtocol.swift
    NORMAL String/StringRangeReplaceableCollection.swift
    NORMAL String/StringStorage.swift
    NORMAL String/StringStorageBridge.swift
    NORMAL String/StringSwitch.swift
    NORMAL String/StringTesting.swift
    NORMAL String/StringUnicodeScalarView.swift
    NORMAL String/StringUTF8Validation.swift
    NORMAL String/StringUTF8View.swift
    NORMAL String/StringUTF16View.swift
    NORMAL String/StringWordBreaking.swift
    NORMAL String/Substring.swift
    NORMAL String/UnavailableStringAPIs.swift

### Unicode
    NORMAL Unicode/ASCII.swift
    NORMAL Unicode/NFC.swift
    NORMAL Unicode/NFD.swift
    NORMAL Unicode/Unicode.swift
    NORMAL Unicode/UnicodeBreakProperty.swift
    NORMAL Unicode/UnicodeData.swift
    NORMAL Unicode/UnicodeEncoding.swift
    NORMAL Unicode/UnicodeHelpers.swift
    NORMAL Unicode/UnicodeParser.swift
  EMBEDDED Unicode/UnicodeScalar.swift
    NORMAL Unicode/UnicodeScalarProperties.swift
    NORMAL Unicode/UnicodeSPI.swift
    NORMAL Unicode/UTF8.swift
    NORMAL Unicode/UTF16.swift
    NORMAL Unicode/UTF32.swift
    NORMAL Unicode/UTFEncoding.swift
    NORMAL Unicode/ValidUTF8Buffer.swift

### Unsafe
  EMBEDDED Unsafe/CTypes.swift
  EMBEDDED Unsafe/LifetimeManager.swift
  EMBEDDED Unsafe/ManagedBuffer.swift
  EMBEDDED Unsafe/Pointer.swift
  EMBEDDED Unsafe/TemporaryAllocation.swift
  EMBEDDED Unsafe/Unmanaged.swift
    NORMAL Unsafe/UnmanagedOpaqueString.swift
    NORMAL Unsafe/UnmanagedString.swift
    NORMAL Unsafe/UnsafeBufferPointerSlice.swift
  EMBEDDED Unsafe/UnsafePointer.swift
  EMBEDDED Unsafe/UnsafeRawPointer.swift
    NORMAL Unsafe/VarArgs.swift

### Misc
  EMBEDDED Assert.swift
  EMBEDDED AssertCommon.swift
  EMBEDDED Availability.swift
  EMBEDDED Bool.swift
  EMBEDDED Builtin.swift
    NORMAL CommandLine.swift
  EMBEDDED Duration.swift
  EMBEDDED DurationProtocol.swift
    NORMAL InputStream.swift
  EMBEDDED Instant.swift
  EMBEDDED KeyPath.swift
    NORMAL LegacyABI.swift
    NORMAL Macros.swift
  EMBEDDED MemoryLayout.swift
    NORMAL MigrationSupport.swift
  EMBEDDED Misc.swift
  EMBEDDED Optional.swift
    NORMAL OutputStream.swift
    NORMAL PlaygroundDisplay.swift
  EMBEDDED Policy.swift
    NORMAL Prespecialize.swift
    NORMAL Print.swift
  EMBEDDED PtrAuth.swift
  EMBEDDED Random.swift
    NORMAL REPL.swift
  EMBEDDED Result.swift
  EMBEDDED Runtime.swift
    NORMAL RuntimeFunctionCounters.swift
    NORMAL Shims.swift
    NORMAL StaticPrint.swift
    NORMAL "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"
)

split_embedded_sources(
  OUT_LIST_EMBEDDED SWIFTLIB_EMBEDDED_GYB_SOURCES
  OUT_LIST_NORMAL SWIFTLIB_GYB_SOURCES

    NORMAL Floats/FloatingPointParsing.swift.gyb
  EMBEDDED Floats/FloatingPointTypes.swift.gyb
    NORMAL Integers/AtomicInt.swift.gyb
  EMBEDDED Integers/IntegerTypes.swift.gyb
  EMBEDDED Integers/LegacyInt128.swift.gyb
  EMBEDDED Unsafe/UnsafeBufferPointer.swift.gyb
  EMBEDDED Unsafe/UnsafeRawBufferPointer.swift.gyb
  EMBEDDED Tuple.swift.gyb
  )

if(SWIFT_STDLIB_ENABLE_VECTOR_TYPES)
  list(APPEND SWIFTLIB_SOURCES SIMD/SIMDVector.swift)
  list(APPEND SWIFTLIB_GYB_SOURCES SIMD/SIMDConcreteOperations.swift.gyb SIMD/SIMDVectorTypes.swift.gyb)

  list(APPEND SWIFTLIB_EMBEDDED_SOURCES SIMD/SIMDVector.swift)
  list(APPEND SWIFTLIB_EMBEDDED_GYB_SOURCES SIMD/SIMDConcreteOperations.swift.gyb SIMD/SIMDVectorTypes.swift.gyb)
endif()

# Freestanding and Linux builds both have failures to resolve.
if(NOT BOOTSTRAPPING_MODE STREQUAL "OFF" AND NOT SWIFT_FREESTANDING_FLAVOR AND NOT SWIFT_HOST_VARIANT_SDK STREQUAL "LINUX")
  list(APPEND SWIFTLIB_SOURCES Reflection/ObjectIdentifier+DebugDescription.swift)
endif()

list(APPEND SWIFTLIB_EMBEDDED_SOURCES
  Embedded/EmbeddedRuntime.swift
  Embedded/EmbeddedStubs.swift
  Embedded/EmbeddedPrint.swift
)

set(GROUP_INFO_JSON_FILE ${CMAKE_CURRENT_SOURCE_DIR}/GroupInfo.json)
set(swift_core_link_flags "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}")
set(swift_core_framework_depends)
set(swift_core_private_link_libraries)
set(swift_stdlib_compile_flags "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}")

if(SWIFT_PRIMARY_VARIANT_SDK STREQUAL "CYGWIN")
  # TODO(compnerd) cache this variable to permit re-configuration
  execute_process(COMMAND "cygpath" "-u" "$ENV{SYSTEMROOT}" OUTPUT_VARIABLE ENV_SYSTEMROOT)
  list(APPEND swift_core_private_link_libraries "${ENV_SYSTEMROOT}/system32/psapi.dll")
elseif(SWIFT_PRIMARY_VARIANT_SDK STREQUAL "FREEBSD")
  find_library(EXECINFO_LIBRARY execinfo)
  list(APPEND swift_core_private_link_libraries ${EXECINFO_LIBRARY})
elseif(SWIFT_PRIMARY_VARIANT_SDK STREQUAL "LINUX")
  if(SWIFT_BUILD_STATIC_STDLIB)
    list(APPEND swift_core_private_link_libraries)
  endif()
elseif(SWIFT_PRIMARY_VARIANT_SDK STREQUAL "WINDOWS")
  list(APPEND swift_core_private_link_libraries shell32;DbgHelp;Synchronization)
endif()

if(NOT "${CMAKE_BUILD_TYPE}" STREQUAL "MinSizeRel")
  list(APPEND swift_stdlib_compile_flags "-Xllvm" "-sil-inline-generics")
  list(APPEND swift_stdlib_compile_flags "-Xllvm" "-sil-partial-specialization")
endif()
if(SWIFT_STDLIB_ENABLE_STDLIBCORE_EXCLUSIVITY_CHECKING)
  list(APPEND swift_stdlib_compile_flags "-enforce-exclusivity=checked")
endif()
if(SWIFT_STDLIB_ENABLE_DEBUG_PRECONDITIONS_IN_RELEASE)
  list(APPEND swift_stdlib_compile_flags "-DSWIFT_STDLIB_ENABLE_DEBUG_PRECONDITIONS_IN_RELEASE")
endif()
set(compile_flags_for_final_build)
if(SWIFT_ENABLE_ARRAY_COW_CHECKS)
  list(APPEND compile_flags_for_final_build "-DCOW_CHECKS_ENABLED")
endif()

# STAGING: Temporarily avoids having to write #fileID in Swift.swiftinterface.
list(APPEND swift_stdlib_compile_flags "-Xfrontend" "-enable-experimental-concise-pound-file")

list(APPEND swift_stdlib_compile_flags "-enable-experimental-feature" "Macros")
list(APPEND swift_stdlib_compile_flags "-enable-experimental-feature" "FreestandingMacros")
list(APPEND swift_stdlib_compile_flags "-enable-experimental-feature" "Extern")
list(APPEND swift_stdlib_compile_flags "-enable-experimental-feature" "BitwiseCopyable")
list(APPEND swift_stdlib_compile_flags "-enable-experimental-feature" "BorrowingSwitch")
list(APPEND swift_stdlib_compile_flags "-enable-experimental-feature" "ConformanceSuppression")

if("${SWIFT_NATIVE_SWIFT_TOOLS_PATH}" STREQUAL "")
  set(swift_bin_dir "${CMAKE_BINARY_DIR}/bin")
  set(swift_lib_dir "${CMAKE_BINARY_DIR}/lib")
else()
  set(swift_bin_dir "${SWIFT_NATIVE_SWIFT_TOOLS_PATH}")
  set(swift_lib_dir "${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/../lib")
endif()

list(APPEND swift_stdlib_compile_flags "-external-plugin-path"
  "${swift_lib_dir}/swift/host/plugins#${swift_bin_dir}/swift-plugin-server")
list(APPEND swift_stdlib_compile_flags "-enable-experimental-feature" "SymbolLinkageMarkers")

set(swift_core_incorporate_object_libraries)
list(APPEND swift_core_incorporate_object_libraries swiftRuntime)
list(APPEND swift_core_incorporate_object_libraries swiftLLVMSupport)
list(APPEND swift_core_incorporate_object_libraries swiftDemangling)
list(APPEND swift_core_incorporate_object_libraries swiftStdlibStubs)
list(APPEND swift_core_incorporate_object_libraries swiftThreading)
if(SWIFT_STDLIB_HAS_COMMANDLINE)
  list(APPEND swift_core_incorporate_object_libraries swiftCommandLineSupport)
endif()

set(swiftCore_common_options
                  IS_STDLIB IS_STDLIB_CORE
                    ${SWIFTLIB_SOURCES}
                  GYB_SOURCES
                    ${SWIFTLIB_GYB_SOURCES}
                  LINK_FLAGS
                    ${swift_core_link_flags}
                  PRIVATE_LINK_LIBRARIES
                    ${swift_core_private_link_libraries}
                  INCORPORATE_OBJECT_LIBRARIES
                    ${swift_core_incorporate_object_libraries}
                  FRAMEWORK_DEPENDS
                    ${swift_core_framework_depends}
                  SWIFT_COMPILE_FLAGS
                    ${swift_stdlib_compile_flags} -Xcc -DswiftCore_EXPORTS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS})

# The copy_shim_headers target dependency is required to let the
# build system know that there's a rule to produce the shims
# directory, but is not sufficient to cause the object file to be rebuilt
# when the shim header changes.  Therefore, we pass both the target
# and the generated directory as dependencies.
set(swiftCore_common_dependencies
    copy_shim_headers "${SWIFTLIB_DIR}/shims" ${GROUP_INFO_JSON_FILE})

if(BOOTSTRAPPING_MODE STREQUAL "BOOTSTRAPPING")

  set(b0_deps symlink-headers-bootstrapping0)
  set(b1_deps symlink-headers-bootstrapping1)

  list(FIND SWIFT_STDLIB_LIBRARY_BUILD_TYPES "SHARED" index_of_shared)
  if(index_of_shared EQUAL -1)
    message(FATAL_ERROR "bootstrapping requires SHARED stdlib build type")
  endif()

  set(swiftCore_common_bootstrapping_options
                    SHARED
                    ${swiftCore_common_options}
                    SDK ${SWIFT_HOST_VARIANT_SDK}
                    ARCHITECTURE ${SWIFT_HOST_VARIANT_ARCH}
                    INSTALL_IN_COMPONENT
                      "never_install")

  # Bootstrapping - stage 0

  add_swift_target_library_single(swiftCore-bootstrapping0 swiftCore
                    ${swiftCore_common_bootstrapping_options}
                    FILE_DEPENDS
                      ${b0_deps} ${swiftCore_common_dependencies}
                    BOOTSTRAPPING 0)

  # Bootstrapping - stage 1

  add_swift_target_library_single(swiftCore-bootstrapping1 swiftCore
                    ${swiftCore_common_bootstrapping_options}
                    FILE_DEPENDS
                      ${b1_deps} ${swiftCore_common_dependencies}
                    BOOTSTRAPPING 1)
endif()

add_swift_target_library(swiftCore
                  ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES}
                  ${swiftCore_common_options}
                  ${compile_flags_for_final_build}
                  FILE_DEPENDS
                    ${swiftCore_common_dependencies}
                  INSTALL_IN_COMPONENT
                    stdlib
                  MACCATALYST_BUILD_FLAVOR
                    zippered
                 )

# Embedded standard library - embedded libraries are built as .swiftmodule only,
# i.e. there is no .o or .a file produced (no binary code is actually produced)
# and only users of a library are going to actually compile any needed code.
if(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB)
  add_custom_target(embedded-stdlib ALL)

  set(SWIFT_ENABLE_REFLECTION OFF)
  set(SWIFT_STDLIB_SUPPORT_BACK_DEPLOYMENT OFF)
  set(SWIFT_STDLIB_STABLE_ABI OFF)
  set(SWIFT_STDLIB_ENABLE_OBJC_INTEROP OFF)

  foreach(entry ${EMBEDDED_STDLIB_TARGET_TRIPLES})
    string(REGEX REPLACE "[ \t]+" ";" list "${entry}")
    list(GET list 0 arch)
    list(GET list 1 mod)
    list(GET list 2 triple)
    
    set(SWIFT_SDK_embedded_ARCH_${arch}_MODULE "${mod}")
    set(SWIFT_SDK_embedded_LIB_SUBDIR "embedded")
    set(SWIFT_SDK_embedded_ARCH_${arch}_TRIPLE "${triple}")
    add_swift_target_library_single(
      embedded-stdlib-${mod}
      swiftCore
      ONLY_SWIFTMODULE
      IS_STDLIB IS_STDLIB_CORE IS_FRAGILE
      ${SWIFTLIB_EMBEDDED_SOURCES}
      GYB_SOURCES ${SWIFTLIB_EMBEDDED_GYB_SOURCES}
      SWIFT_COMPILE_FLAGS
        ${swift_stdlib_compile_flags} -Xcc -D__MACH__ -Xcc -D__APPLE__ -Xcc -ffreestanding -enable-experimental-feature Embedded
        -Xfrontend -enable-ossa-modules
      MODULE_DIR "${CMAKE_BINARY_DIR}/lib/swift/embedded"
      SDK "embedded"
      ARCHITECTURE "${arch}"
      FILE_DEPENDS ${swiftCore_common_dependencies}
      INSTALL_IN_COMPONENT stdlib
      )
    add_dependencies(embedded-stdlib embedded-stdlib-${mod})
  endforeach()
endif()
