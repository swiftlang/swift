# TODO: Hook up the CommandLine support sources

gyb_expand(AtomicInt.swift.gyb AtomicInt.swift
  FLAGS "-DCMAKE_SIZEOF_VOID_P=${SwiftCore_SIZEOF_POINTER}")

gyb_expand(FloatingPointParsing.swift.gyb FloatingPointParsing.swift
  FLAGS "-DCMAKE_SIZEOF_VOID_P=${SwiftCore_SIZEOF_POINTER}")

gyb_expand(FloatingPointTypes.swift.gyb FloatingPointTypes.swift
  FLAGS "-DCMAKE_SIZEOF_VOID_P=${SwiftCore_SIZEOF_POINTER}")

gyb_expand(IntegerTypes.swift.gyb IntegerTypes.swift
  FLAGS "-DCMAKE_SIZEOF_VOID_P=${SwiftCore_SIZEOF_POINTER}")

gyb_expand(LegacyInt128.swift.gyb LegacyInt128.swift
  FLAGS "-DCMAKE_SIZEOF_VOID_P=${SwiftCore_SIZEOF_POINTER}")

gyb_expand(UnsafeBufferPointer.swift.gyb UnsafeBufferPointer.swift
  FLAGS "-DCMAKE_SIZEOF_VOID_P=${SwiftCore_SIZEOF_POINTER}")

gyb_expand(UnsafeRawBufferPointer.swift.gyb UnsafeRawBufferPointer.swift
  FLAGS "-DCMAKE_SIZEOF_VOID_P=${SwiftCore_SIZEOF_POINTER}")

gyb_expand(Tuple.swift.gyb Tuple.swift
  FLAGS "-DCMAKE_SIZEOF_VOID_P=${SwiftCore_SIZEOF_POINTER}")

# These sources are not strictly sorted alphabetically because the compiler
# crashes if they are.
add_library(swiftCore
  Algorithm.swift
  ArrayBody.swift
  ArrayBuffer.swift
  ArrayBufferProtocol.swift
  ArrayCast.swift
  Array.swift
  ArrayShared.swift
  ArraySlice.swift
  ArrayType.swift
  ASCII.swift
  Assert.swift
  AssertCommon.swift
  BidirectionalCollection.swift
  Bitset.swift
  Bool.swift
  BridgeObjectiveC.swift
  BridgeStorage.swift
  BridgingBuffer.swift
  Builtin.swift
  BuiltinMath.swift
  Character.swift
  CocoaArray.swift
  Codable.swift
  Collection.swift
  CollectionAlgorithms.swift
  Comparable.swift
  CompilerProtocols.swift
  Sendable.swift
  ContiguousArray.swift
  ContiguouslyStored.swift
  ClosedRange.swift
  ContiguousArrayBuffer.swift
  CString.swift
  CTypes.swift
  DebuggerSupport.swift
  Dictionary.swift
  DictionaryBridging.swift
  DictionaryBuilder.swift
  DictionaryCasting.swift
  DictionaryStorage.swift
  DictionaryVariant.swift
  DiscontiguousSlice.swift
  DropWhile.swift
  Dump.swift
  EmptyCollection.swift
  EnumeratedSequence.swift
  Equatable.swift
  ErrorType.swift
  ExistentialCollection.swift
  Filter.swift
  FlatMap.swift
  Flatten.swift
  FloatingPoint.swift
  Hashable.swift
  AnyHashable.swift # ORDER DEPENDENCY
  Hasher.swift
  Hashing.swift
  HashTable.swift
  Identifiable.swift
  Indices.swift
  InlineArray.swift
  InputStream.swift
  IntegerParsing.swift
  Integers.swift
  Join.swift
  KeyPath.swift
  KeyValuePairs.swift
  LazyCollection.swift
  LazySequence.swift
  LegacyABI.swift
  LifetimeManager.swift
  Macros.swift
  ManagedBuffer.swift
  Map.swift
  MemoryLayout.swift
  UnicodeScalar.swift # ORDER DEPENDENCY: Must precede Mirrors.swift
  Mirrors.swift
  Misc.swift
  MutableCollection.swift
  NativeDictionary.swift
  NativeSet.swift
  NewtypeWrapper.swift
  NFC.swift
  NFD.swift
  ObjectIdentifier.swift
  Optional.swift
  OptionSet.swift
  OutputStream.swift
  Pointer.swift
  Policy.swift
  PrefixWhile.swift
  Prespecialize.swift
  Print.swift
  PtrAuth.swift
  Random.swift
  RandomAccessCollection.swift
  Range.swift
  RangeReplaceableCollection.swift
  RangeSet.swift
  RangeSetRanges.swift
  ReflectionMirror.swift
  Repeat.swift
  REPL.swift
  Result.swift
  Reverse.swift
  Runtime.swift
  RuntimeFunctionCounters.swift
  SipHash.swift
  Sequence.swift
  SequenceAlgorithms.swift
  Set.swift
  SetAlgebra.swift
  SetAnyHashableExtensions.swift
  SetBridging.swift
  SetBuilder.swift
  SetCasting.swift
  SetStorage.swift
  SetVariant.swift
  ShadowProtocols.swift
  Shims.swift
  Slice.swift
  SmallString.swift
  Sort.swift
  Span/Span.swift
  Span/RawSpan.swift
  Span/MutableSpan.swift
  Span/MutableRawSpan.swift
  Span/OutputSpan.swift
  Span/OutputRawSpan.swift
  StaticString.swift
  StaticPrint.swift
  Stride.swift
  StringHashable.swift  # ORDER DEPENDENCY: Must precede String.swift
  String.swift
  StringBreadcrumbs.swift
  StringBridge.swift
  StringCharacterView.swift
  StringComparable.swift
  StringComparison.swift
  StringCreate.swift
  StringGuts.swift
  StringGutsSlice.swift
  StringGutsRangeReplaceable.swift
  StringObject.swift
  StringProtocol.swift
  StringIndex.swift
  StringIndexConversions.swift
  StringIndexValidation.swift
  StringInterpolation.swift
  StringLegacy.swift
  StringNormalization.swift
  StringRangeReplaceableCollection.swift
  StringStorage.swift
  StringStorageBridge.swift
  StringSwitch.swift
  StringTesting.swift
  StringUnicodeScalarView.swift
  StringUTF16View.swift
  StringUTF8View.swift
  StringUTF8Validation.swift
  StringWordBreaking.swift
  Substring.swift
  SwiftNativeNSArray.swift
  TemporaryAllocation.swift
  ThreadLocalStorage.swift
  UIntBuffer.swift
  UnavailableStringAPIs.swift
  UnicodeData.swift
  UnicodeEncoding.swift
  UnicodeBreakProperty.swift
  UnicodeHelpers.swift
  UnicodeParser.swift
  UnicodeScalarProperties.swift
  CharacterProperties.swift # ORDER DEPENDENCY: UnicodeScalarProperties.swift
  UnicodeSPI.swift
  Unmanaged.swift
  UnmanagedOpaqueString.swift
  UnmanagedString.swift
  UnsafePointer.swift
  UnsafeRawPointer.swift
  UTFEncoding.swift
  UTF8.swift
  UTF8EncodingError.swift
  UTF8Span.swift
  UTF8SpanBits.swift
  UTF8SpanComparisons.swift
  UTF8SpanFundamentals.swift
  UTF8SpanInternalHelpers.swift
  UTF8SpanIterators.swift
  UTF16.swift
  UTF32.swift
  Unicode.swift # ORDER DEPENDENCY: must follow new unicode support
  StringGraphemeBreaking.swift # ORDER DEPENDENCY: Must follow UTF16.swift
  ValidUTF8Buffer.swift
  WriteBackMutableSlice.swift
  MigrationSupport.swift

  Availability.swift
  CollectionDifference.swift
  CollectionOfOne.swift
  Diffing.swift
  Duration.swift
  DurationProtocol.swift
  FloatingPointRandom.swift
  Instant.swift
  Int128.swift
  Mirror.swift
  PlaygroundDisplay.swift
  SwiftifyImport.swift
  CommandLine.swift
  SliceBuffer.swift
  StaticBigInt.swift
  UInt128.swift
  UnfoldSequence.swift
  UnsafeBufferPointerSlice.swift
  VarArgs.swift
  Zip.swift

  "${PROJECT_SOURCE_DIR}/linker-support/magic-symbols-for-install-name.c"
  "${CMAKE_CURRENT_BINARY_DIR}/AtomicInt.swift"
  "${CMAKE_CURRENT_BINARY_DIR}/FloatingPointParsing.swift"
  "${CMAKE_CURRENT_BINARY_DIR}/FloatingPointTypes.swift"
  "${CMAKE_CURRENT_BINARY_DIR}/IntegerTypes.swift"
  "${CMAKE_CURRENT_BINARY_DIR}/LegacyInt128.swift"
  "${CMAKE_CURRENT_BINARY_DIR}/UnsafeBufferPointer.swift"
  "${CMAKE_CURRENT_BINARY_DIR}/UnsafeRawBufferPointer.swift"
  "${CMAKE_CURRENT_BINARY_DIR}/Tuple.swift")

# https://github.com/swiftlang/swift/issues/77705 - Freestanding and Linux/Android builds both have failures to resolve.
if(NOT LINUX AND NOT ANDROID)
  target_sources(swiftCore PRIVATE ObjectIdentifier+DebugDescription.swift)
endif()

if(SwiftCore_ENABLE_COMMANDLINE_SUPPORT)
  target_sources(swiftCore PRIVATE CommandLine.swift)
  target_link_libraries(swiftCore PRIVATE swiftCommandLineSupport)
endif()

if(SwiftCore_ENABLE_VECTOR_TYPES)
  gyb_expand(SIMDConcreteOperations.swift.gyb
    SIMDConcreteOperations.swift
    FLAGS "-DCMAKE_SIZEOF_VOID_P=${SwiftCore_SIZEOF_POINTER}")
  gyb_expand(SIMDVectorTypes.swift.gyb
    SIMDVectorTypes.swift
    FLAGS "-DCMAKE_SIZEOF_VOID_P=${SwiftCore_SIZEOF_POINTER}")

  target_sources(swiftCore PRIVATE
    SIMDVector.swift
    "${CMAKE_CURRENT_BINARY_DIR}/SIMDConcreteOperations.swift"
    "${CMAKE_CURRENT_BINARY_DIR}/SIMDVectorTypes.swift")
endif()

set_target_properties(swiftCore PROPERTIES
  Swift_MODULE_NAME Swift
  LINKER_LANGUAGE CXX)

target_compile_definitions(swiftCore
  PRIVATE
    $<$<BOOL:${SwiftCore_ENABLE_REFLECTION}>:-DSWIFT_ENABLE_REFLECTION>
    $<$<BOOL:${SwiftCore_ENABLE_COMPACT_ABSOLUTE_FUNCTION_POINTERS}>:-DSWIFT_COMPACT_ABSOLUTE_FUNCTION_POINTER>
    $<$<COMPILE_LANGUAGE:C,CXX>:-DSWIFT_TARGET_LIBRARY_NAME=swiftCore>
  INTERFACE
    $<$<BOOL:${SwiftCore_ENABLE_VECTOR_TYPES}>:-DSWIFT_STDLIB_ENABLE_VECTOR_TYPES>)

target_compile_options(swiftCore PRIVATE
  "$<$<AND:$<COMPILE_LANGUAGE:Swift>,$<BOOL:${BUILD_SHARED_LIBS}>>:SHELL:-Xcc -DswiftCore_EXPORTS>"
  # STAGING: Temporarily avoids having to write #fileID in Swift.swiftinterface.
  # see also 327ea8bce2d1107a847d444651b19ca6a2901c9e
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enable-experimental-concise-pound-file>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature AddressableParameters>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature AddressableTypes>"
  $<$<COMPILE_LANGUAGE:Swift>:-parse-stdlib>
  $<$<COMPILE_LANGUAGE:Swift>:-nostdimport>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -group-info-path -Xfrontend ${CMAKE_CURRENT_SOURCE_DIR}/GroupInfo.json>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-objc-attr-requires-foundation-module>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -require-explicit-availability=ignore>"
  "$<$<AND:$<PLATFORM_ID:Darwin>,$<COMPILE_LANGUAGE:Swift>>:SHELL:-Xfrontend -previous-module-installname-map-file -Xfrontend ${CMAKE_CURRENT_SOURCE_DIR}/PreviousModuleInstallName.json>")
if(NOT "${CMAKE_BUILD_TYPE}" STREQUAL "MinSizeRel")
  # Using these in MinSizeRel would result in a 15% increase in the binary size
  target_compile_options(swiftCore PRIVATE
    "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xllvm -sil-inline-generics>"
    "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xllvm -sil-partial-specialization>")
endif()

# FIXME: Why is this not implicitly in the interface flags?
target_include_directories(swiftCore INTERFACE
  "$<$<COMPILE_LANGUAGE:Swift>:$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${SwiftCore_INSTALL_SWIFTMODULEDIR}>>")

target_link_libraries(swiftCore
  PRIVATE
    swiftRuntime
    swiftLLVMSupport
    swiftDemangling
    swiftStdlibStubs
    swiftThreading
    $<$<NOT:$<PLATFORM_ID:Darwin>>:swiftrt$<$<PLATFORM_ID:Windows>:T>>
  PUBLIC
    swiftShims)

string(TOLOWER "${SwiftCore_OBJECT_FORMAT}" SwiftCore_OBJECT_FORMAT_lc)
if("${SwiftCore_OBJECT_FORMAT_lc}" STREQUAL "elf")
  target_link_libraries(swiftCore INTERFACE
    swiftrt)
elseif("${SwiftCore_OBJECT_FORMAT_lc}" STREQUAL "coff")
  if(BUILD_SHARED_LIBS)
    target_link_libraries(swiftCore INTERFACE
      swiftrt)
  else()
    target_link_libraries(swiftCore INTERFACE
      swiftrtT)
  endif()
endif()

target_link_options(swiftCore PUBLIC
  $<$<LINK_LANGUAGE:Swift>:-nostartfiles>)

if(NOT POLICY CMP0157)
  target_compile_options(swiftCore PRIVATE
    $<TARGET_OBJECTS:swiftRuntime>
    $<TARGET_OBJECTS:swiftLLVMSupport>
    $<TARGET_OBJECTS:swiftDemangling>
    $<TARGET_OBJECTS:swiftStdlibStubs>
    $<TARGET_OBJECTS:swiftThreading>)
endif()

if(NOT ANDROID AND NOT APPLE AND NOT LINUX AND NOT WIN32 AND UNIX)
  find_library(EXECINFO_LIBRARY execinfo)
  target_link_libraries(swiftCore PRIVATE "${EXECINFO_LIBRARY}")
endif()

if(APPLE AND BUILD_SHARED_LIBS)
  target_link_options(swiftCore PRIVATE "SHELL:-Xlinker -headerpad_max_install_names")
endif()

install(TARGETS swiftCore
  EXPORT SwiftCoreTargets
  COMPONENT SwiftCore_runtime
  ARCHIVE DESTINATION "${SwiftCore_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftCore_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftCore)
install_swift_interface(swiftCore)

# Configure plist creation for Darwin platforms.
generate_plist("${CMAKE_PROJECT_NAME}" "${CMAKE_PROJECT_VERSION}" swiftCore)
embed_manifest(swiftCore)

include("${SwiftCore_VENDOR_MODULE_DIR}/swiftCore.cmake" OPTIONAL)

# TODO: Embedded SwiftCore builds
# FIXME: Embedded builds should be separate CMake invocations because they are
#        building for a different environment. I'm not sure how CMake will
#        handle their build model though, so we'll continue to cram them in here
#        as they are for now, but should eventually tie into the rest of the
#        build graph normally.
