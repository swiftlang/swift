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

set_target_properties(swiftCore PROPERTIES Swift_MODULE_NAME Swift)

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

target_compile_options(swiftCore PRIVATE
  $<$<COMPILE_LANGUAGE:Swift>:-parse-stdlib>
  $<$<COMPILE_LANGUAGE:Swift>:-nostdimport>
  $<$<COMPILE_LANGUAGE:Swift>:-explicit-module-build>
  $<$<COMPILE_LANGUAGE:Swift>:-disable-autolinking-runtime-compatibility-dynamic-replacements>
  $<$<COMPILE_LANGUAGE:Swift>:-no-link-objc-runtime>
  $<$<COMPILE_LANGUAGE:Swift>:-warn-implicit-overrides>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-library-level api>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-runtime-compatibility-version none>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature NoncopyableGenerics2>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature SuppressedAssociatedTypes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature SE427NoInferenceOnExtension>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature AllowUnsafeAttribute>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature NonescapableTypes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature MemberImportVisibility>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature TypedThrows>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature Macros>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature FreestandingMacros>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature BitwiseCopyable>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature Extern>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature DebugDescriptionMacro>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -group-info-path -Xfrontend ${CMAKE_CURRENT_SOURCE_DIR}/GroupInfo.json>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enable-experimental-concise-pound-file>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-autolinking-runtime-compatibility-concurrency>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-objc-attr-requires-foundation-module>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -require-explicit-availability=ignore>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enforce-exclusivity=unchecked>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enable-ossa-modules>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -target-min-inlining-version -Xfrontend min>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enable-lexical-lifetimes=false>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -empty-abi-descriptor>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-concurrency-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-backtracing-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-string-processing-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -prespecialize-generic-metadata>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xllvm -sil-inline-generics>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xllvm -sil-partial-specialization>")

target_compile_definitions(swiftCore PRIVATE
  $<$<BOOL:${SwiftCore_ENABLE_REFLECTION}>:-DSWIFT_ENABLE_REFLECTION>)
target_compile_options(swiftCore PRIVATE
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc -DswiftCore_EXPORTS>")

target_link_libraries(swiftCore PRIVATE swiftShims)
target_link_libraries(swiftCore
  PRIVATE
    swiftRuntime
    swiftLLVMSupport
    swiftDemangling
    swiftStdlibStubs
    swiftThreading)
if(NOT POLICY CMP0157)
  target_compile_options(swiftCore PRIVATE
    $<TARGET_OBJECTS:swiftRuntime>
    $<TARGET_OBJECTS:swiftLLVMSupport>
    $<TARGET_OBJECTS:swiftDemangling>
    $<TARGET_OBJECTS:swiftStdlibStubs>
    $<TARGET_OBJECTS:swiftThreading>)
endif()

if(WIN32)
  target_link_libraries(swiftCore PRIVATE shell32 DbgHelp Synchronization)
elseif(NOT APPLE AND NOT LINUX AND NOT ANDROID AND UNIX)
  find_library(EXECINFO_LIBRARY execinfo)
  target_link_libraries(swiftCore PRIVATE "${EXECINFO_LIBRARY}")
endif()

install(TARGETS swiftCore)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/Swift.swiftmodule"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/swift/Swift.swiftmodule"
  RENAME "${SwiftCore_MODULE_TRIPLE}.swiftmodule")
emit_swift_interface(swiftCore)
install_swift_interface(swiftCore)

# TODO: Embedded SwiftCore builds
# FIXME: Embedded builds should be separate CMake invocations because they are
#        building for a different environment. I'm not sure how CMake will
#        handle their build model though, so we'll continue to cram them in here
#        as they are for now, but should eventually tie into the rest of the
#        build graph normally.
