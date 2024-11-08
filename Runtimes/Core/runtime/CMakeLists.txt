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

# TODO: Probably worth considering putting half of these in a RuntimeConfig.h.in
#       file rather than pushing them through macro flags.
target_compile_definitions(swiftRuntime
  PRIVATE
    -DswiftCore_EXPORTS
    -DSWIFT_RUNTIME
    -DSWIFT_TARGET_LIBRARY_NAME=swiftRuntime
    $<$<BOOL:${SwiftCore_ENABLE_BACKTRACING}>:-DSWIFT_ENABLE_BACKTRACING>
    $<$<BOOL:${SwiftCore_BACKTRACER_PATH}>:-DSWIFT_RUNTIME_FIXED_BACKTRACER_PATH="${SwiftCore_BACKTRACER_PATH}">)

target_include_directories(swiftRuntime PRIVATE
  "${PROJECT_BINARY_DIR}/include"
  "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(swiftRuntime PRIVATE swiftShims)

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

if(SwiftCore_ENABLE_OBJC_INTEROP)
  target_sources(swiftRuntime PRIVATE
    ErrorObject.mm
    SwiftObject.mm
    SwiftValue.mm
    ReflectionMirrorObjC.mm
    ObjCRuntimeGetImageNameFromClass.mm)
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
  install(TARGETS swiftrt DESTINATION "${CMAKE_INSTALL_LIBDIR}/swift")
elseif("${SwiftCore_OBJECT_FORMAT}" STREQUAL "coffx")
  add_library(swiftrt OBJECT SwiftRT-COFF.cpp)
  target_link_libraries(swiftrt PRIVATE swiftShims)
  install(TARGETS swiftrt DESTINATION "${CMAKE_INSTALL_LIBDIR}/swift")
elseif(NOT "${SwiftCore_OBJECT_FORMAT}" STREQUAL "x")
  message(SEND_ERROR "Unknown object format '${SwiftCore_OBJECT_FORMAT}'")
endif()
