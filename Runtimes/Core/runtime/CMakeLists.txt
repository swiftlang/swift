# TODO: clean this up so it's less Apple-specific.
# Detect B&I builds.
set(SWIFT_BNI_OS_BUILD FALSE)
set(SWIFT_BNI_XCODE_BUILD FALSE)
if(DEFINED ENV{RC_XBS})
  if((NOT DEFINED ENV{RC_XCODE} OR NOT "$ENV{RC_XCODE}") AND (NOT DEFINED ENV{RC_PLAYGROUNDS} OR NOT "$ENV{RC_PLAYGROUNDS}"))
    set(SWIFT_BNI_OS_BUILD TRUE)
  else()
    set(SWIFT_BNI_XCODE_BUILD TRUE)
  endif()
endif()

configure_file("CMakeConfig.h.in"
  "${PROJECT_BINARY_DIR}/include/swift/Runtime/CMakeConfig.h"
  ESCAPE_QUOTES @ONLY)

add_library(swiftRuntime OBJECT
    "${PROJECT_SOURCE_DIR}/CompatibilityOverride/CompatibilityOverride.cpp"
    AnyHashableSupport.cpp
    Array.cpp
    AutoDiffSupport.cpp
    Bincompat.cpp
    BytecodeLayouts.cpp
    Casting.cpp
    CrashReporter.cpp
    Demangle.cpp
    DynamicCast.cpp
    Enum.cpp
    EnvironmentVariables.cpp
    ErrorObjectCommon.cpp
    ErrorObjectNative.cpp
    Errors.cpp
    ErrorDefaultImpls.cpp
    Exception.cpp
    Exclusivity.cpp
    ExistentialContainer.cpp
    Float16Support.cpp
    FoundationSupport.cpp
    FunctionReplacement.cpp
    GenericMetadataBuilder.cpp
    Heap.cpp
    HeapObject.cpp
    ImageInspectionCommon.cpp
    ImageInspectionMachO.cpp
    ImageInspectionELF.cpp
    ImageInspectionCOFF.cpp
    ImageInspectionStatic.cpp
    ImageInspectionWasm.cpp
    SymbolInfo.cpp
    KeyPaths.cpp
    KnownMetadata.cpp
    LibPrespecialized.cpp
    Metadata.cpp
    MetadataLookup.cpp
    Numeric.cpp
    Once.cpp
    Paths.cpp
    Portability.cpp
    ProtocolConformance.cpp
    RefCount.cpp
    ReflectionMirror.cpp
    RuntimeInvocationsTracking.cpp
    SwiftDtoa.cpp
    SwiftTLSContext.cpp
    ThreadingError.cpp
    Tracing.cpp
    AccessibleFunction.cpp
    Win32.cpp)

if(SwiftCore_ENABLE_RUNTIME_LEAK_CHECKER)
  target_sources(swiftRuntime PRIVATE
    Leaks.mm)
endif()

# runtime/Paths.cpp uses `SWIFT_ARCH` and `SWIFT_LIB_SUBDIR` to find the
# subdirectory under `libexec` in which to find the backtracer.
# Fail the configuration if it isn't set appropriately.
# TODO: In the future, we might consider moving the backtracer up to
# `libexec/swift` and drop the platform/architecture specific subdirectories
if(SwiftCore_ENABLE_FILESYSTEM_SUPPORT AND (NOT SwiftCore_ARCH_SUBDIR OR NOT SwiftCore_PLATFORM_SUBDIR))
  message(SEND_ERROR "Filesystem support requires setting `SwiftCore_ARCH_SUBDIR` and `SwiftCore_PLATFORM_SUBDIR`")
endif()

# TODO: Probably worth considering putting half of these in a RuntimeConfig.h.in
#       file rather than pushing them through macro flags.
target_compile_definitions(swiftRuntime
  PRIVATE
    -DSWIFT_RUNTIME
    -DSWIFT_TARGET_LIBRARY_NAME=swiftRuntimeCore
    $<$<BOOL:${BUILD_SHARED_LIBS}>:-DswiftCore_EXPORTS>
    $<$<BOOL:${SwiftCore_ENABLE_BACKTRACING}>:-DSWIFT_ENABLE_BACKTRACING>
    $<$<BOOL:${SwiftCore_ENABLE_OVERRIDABLE_RETAIN_RELEASE}>:-DSWIFT_STDLIB_OVERRIDABLE_RETAIN_RELEASE>
    $<$<BOOL:${SwiftCore_BACKTRACER_PATH}>:-DSWIFT_RUNTIME_FIXED_BACKTRACER_PATH="${SwiftCore_BACKTRACER_PATH}">
    $<$<BOOL:${SwiftCore_ENABLE_STDLIB_TRACING}>:-DSWIFT_STDLIB_TRACING>
    $<$<BOOL:${SwiftCore_ENABLE_SHORT_MANGLING_LOOKUPS}>:-DSWIFT_STDLIB_SHORT_MANGLING_LOOKUPS>
    $<$<BOOL:${SwiftCore_HAS_ASL}>:-DSWIFT_STDLIB_HAS_ASL>
    $<$<BOOL:${SwiftCore_ENABLE_TYPE_PRINTING}>:-DSWIFT_STDLIB_HAS_TYPE_PRINTING>
    $<$<BOOL:${SwiftCore_HAS_DLADDR}>:-DSWIFT_STDLIB_HAS_DLADDR>
    $<$<BOOL:${SwiftCore_HAS_DLSYM}>:-DSWIFT_STDLIB_HAS_DLSYM>
    $<$<BOOL:${SwiftCore_ENABLE_FILESYSTEM_SUPPORT}>:-DSWIFT_STDLIB_HAS_FILESYSTEM>
    $<$<BOOL:${SwiftCore_ENABLE_FILESYSTEM_SUPPORT}>:-DSWIFT_ARCH="${SwiftCore_ARCH_SUBDIR}">
    $<$<BOOL:${SwiftCore_ENABLE_FILESYSTEM_SUPPORT}>:-DSWIFT_LIB_SUBDIR="${SwiftCore_PLATFORM_SUBDIR}">
    $<$<BOOL:${SwiftCore_ENABLE_FATALERROR_BACKTRACE}>:-DSWIFT_STDLIB_SUPPORTS_BACKTRACE_REPORTING>
  PUBLIC
    $<$<BOOL:${SwiftCore_ENABLE_VECTOR_TYPES}>:-DSWIFT_STDLIB_ENABLE_VECTOR_TYPES>
    $<$<BOOL:${SwiftCore_ENABLE_OS_TRACE_LAZY_INIT}>:-DSWIFT_USE_OS_TRACE_LAZY_INIT>)

target_include_directories(swiftRuntime PRIVATE
  "${PROJECT_BINARY_DIR}/include"
  "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(swiftRuntime PRIVATE
  $<$<PLATFORM_ID:Windows>:User32>
  $<$<PLATFORM_ID:Windows>:ShLwApi>
  swiftShims
  swiftDemangling)

# FIXME: Refactor so that we're not pulling sources from the compiler files
target_sources(swiftRuntime PRIVATE
  "${SwiftCore_SWIFTC_SOURCE_DIR}/lib/Threading/ThreadSanitizer.cpp")

# FIXME: Private.h uses `Demangler.h` and `TypeLookupError.h` from the compiler
#        headers. We should split out the parts that are needed by the runtime
#        to avoid pulling in headers from the compiler.
target_include_directories(swiftRuntime PRIVATE
  "${SwiftCore_SWIFTC_SOURCE_DIR}/include")

if(SwiftCore_ENABLE_BACKTRACING)
  target_sources(swiftRuntime PRIVATE
    Backtrace.cpp
    BacktraceUtils.cpp
    CrashHandlerMacOS.cpp
    CrashHandlerLinux.cpp)
endif()

target_sources(swiftRuntime PRIVATE
  ErrorObject.mm
  SwiftObject.mm)

if(SwiftCore_ENABLE_OBJC_INTEROP)
  target_sources(swiftRuntime PRIVATE
    SwiftValue.mm
    ReflectionMirrorObjC.mm
    ObjCRuntimeGetImageNameFromClass.mm)
endif()

if(NOT BUILD_SHARED_LIBS)
  install(TARGETS swiftRuntime
    EXPORT SwiftCoreTargets
    COMPONENT SwiftCore_runtime)
endif()

string(TOLOWER "${SwiftCore_OBJECT_FORMAT}x" SwiftCore_OBJECT_FORMAT)
if("${SwiftCore_OBJECT_FORMAT}" STREQUAL "elfx")
  add_library(swiftrt OBJECT SwiftRT-ELF-WASM.cpp)
  target_compile_definitions(swiftrt PRIVATE
    -DSWIFT_ENABLE_BACKTRACING=$<BOOL:${SwiftCore_ENABLE_BACKTRACING}>)
  target_include_directories(swiftrt PRIVATE
    "${SwiftCore_SWIFTC_SOURCE_DIR}/include"
    "${PROJECT_BINARY_DIR}/include")
  target_link_libraries(swiftrt PRIVATE swiftShims)
  # The driver requires that swiftrt.o is under
  # `usr/lib/(swift|swift_static)/<platform>/<arch>` regardless of whether the
  # other files are under the platform and architecture subdirectories:
  # https://github.com/swiftlang/swift-driver/blob/f66e33575150cc778289b5f573218c7a0c70bab6/Sources/SwiftDriver/Jobs/GenericUnixToolchain%2BLinkerSupport.swift#L186
  install(FILES $<TARGET_OBJECTS:swiftrt>
    COMPONENT SwiftCore_runtime
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>/${SwiftCore_PLATFORM_SUBDIR}/${SwiftCore_ARCH_SUBDIR}"
    RENAME swiftrt.o)

  install(TARGETS swiftrt
    EXPORT SwiftCoreTargets
    COMPONENT SwiftCore_runtime)
elseif("${SwiftCore_OBJECT_FORMAT}" STREQUAL "coffx")
  add_library(swiftrtT OBJECT SwiftRT-COFF.cpp)
  target_compile_definitions(swiftrtT PRIVATE
    SWIFT_STATIC_STDLIB)
  target_link_libraries(swiftrtT PRIVATE swiftShims)
  install(FILES $<TARGET_OBJECTS:swiftrtT>
    COMPONENT SwiftCore_runtime
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/swift/${SwiftCore_PLATFORM_SUBDIR}/${SwiftCore_ARCH_SUBDIR}"
    RENAME swiftrtT.obj)

  add_library(swiftrt OBJECT SwiftRT-COFF.cpp)
  target_link_libraries(swiftrt PRIVATE swiftShims)
  install(FILES $<TARGET_OBJECTS:swiftrt>
    COMPONENT SwiftCore_runtime
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/swift/${SwiftCore_PLATFORM_SUBDIR}/${SwiftCore_ARCH_SUBDIR}"
    RENAME swiftrt.obj)

  install(TARGETS swiftrtT swiftrt
    EXPORT SwiftCoreTargets
    COMPONENT SwiftCore_runtime)
elseif(NOT "${SwiftCore_OBJECT_FORMAT}" STREQUAL "x")
  message(SEND_ERROR "Unknown object format '${SwiftCore_OBJECT_FORMAT}'")
endif()
