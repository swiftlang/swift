cmake_minimum_required(VERSION 3.29)
# TODO before requiring CMake 4.1 or later
# and/or enforcing CMP0195, please check/update
# the implementation  of `emit_swift_interface`
# in `EmitSwiftInterface.cmake`
# to ensure it keeps laying down nested swiftmodule folders

if(POLICY CMP0157 AND CMAKE_Swift_COMPILER_USE_OLD_DRIVER)
  cmake_policy(SET CMP0157 OLD)
endif()

list(APPEND CMAKE_MODULE_PATH
  "${CMAKE_SOURCE_DIR}/../cmake/modules"
  "${CMAKE_SOURCE_DIR}/../../cmake/modules")

include(SwiftProjectVersion)
project(SwiftRuntime
  LANGUAGES Swift CXX
  VERSION ${SWIFT_RUNTIME_VERSION})

if(NOT PROJECT_IS_TOP_LEVEL)
  message(SEND_ERROR "Swift Runtime must build as a standalone project")
endif()

include(AsmSupport)
enable_asm_language()

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)
set(CMAKE_POSITION_INDEPENDENT_CODE YES)

set(${PROJECT_NAME}_SWIFTC_SOURCE_DIR
  "${PROJECT_SOURCE_DIR}/../../../"
  CACHE FILEPATH "Path to the root source directory of the Swift compiler")

# Hook point for vendor-specific extensions to the build system
# Allowed extension points:
#   - DefaultSettings.cmake
#   - Settings.cmake
set(${PROJECT_NAME}_VENDOR_MODULE_DIR "${CMAKE_SOURCE_DIR}/../cmake/modules/vendor"
  CACHE FILEPATH "Location for private build system extension")

find_package(SwiftCore REQUIRED)
find_package(SwiftOverlay REQUIRED)
find_package(SwiftCxxOverlay REQUIRED)

if(APPLE)
find_package(SwiftDarwin)
endif()

include(GNUInstallDirs)

include(AvailabilityMacros)
include(EmitSwiftInterface)
include(InstallSwiftInterface)
include(PlatformInfo)
include(gyb)
include(ResourceEmbedding)
include(CatalystSupport)

option(${PROJECT_NAME}_INSTALL_NESTED_SUBDIR "Install libraries under a platform and architecture subdirectory" ON)
set(${PROJECT_NAME}_INSTALL_LIBDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${${PROJECT_NAME}_INSTALL_NESTED_SUBDIR}>:/${${PROJECT_NAME}_PLATFORM_SUBDIR}/${${PROJECT_NAME}_ARCH_SUBDIR}>" CACHE STRING "")
set(${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${${PROJECT_NAME}_INSTALL_NESTED_SUBDIR}>:/${${PROJECT_NAME}_PLATFORM_SUBDIR}>" CACHE STRING "")

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/Settings.cmake" OPTIONAL)

option(${PROJECT_NAME}_ENABLE_BACKTRACING "Enable backtracing support"
  ${SwiftCore_ENABLE_BACKTRACING})

option(${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION "Generate ABI resilient runtime libraries"
  ${SwiftCore_ENABLE_LIBRARY_EVOLUTION})

option(${PROJECT_NAME}_ENABLE_PRESPECIALIZATION "Enable generic metadata prespecialization"
  ${SwiftCore_ENABLE_PRESPECIALIZATION})

add_compile_options(
  $<$<COMPILE_LANGUAGE:Swift>:-explicit-module-build>
  $<$<COMPILE_LANGUAGE:Swift>:-nostdlibimport>
  $<$<COMPILE_LANGUAGE:Swift>:-parse-stdlib>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-swift-version 5>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Werror StrictMemorySafety>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enforce-exclusivity=unchecked>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -target-min-inlining-version -Xfrontend min>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-string-processing-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature NoncopyableGenerics2>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature SuppressedAssociatedTypes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature SE427NoInferenceOnExtension>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature NonescapableTypes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature LifetimeDependence>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature InoutLifetimeDependence>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature LifetimeDependenceMutableAccessors>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-disable-upcoming-feature MemberImportVisibility>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enable-lexical-lifetimes=false>"
  "$<$<COMPILE_LANGUAGE:Swift>:-warn-implicit-overrides>"
  "$<$<COMPILE_LANGUAGE:Swift>:-cxx-interoperability-mode=default>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -experimental-spi-only-imports>"
  "$<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION}>,$<COMPILE_LANGUAGE:Swift>>:-enable-library-evolution>"
  "$<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_PRESPECIALIZATION}>,$<COMPILE_LANGUAGE:Swift>>:SHELL:-Xfrontend -prespecialize-generic-metadata>")

if(WIN32 AND ${CMAKE_SYSTEM_PROCESSOR} MATCHES "X86")
  # Turn on /safeseh for 32-bit x86 or we won't be able to link
  add_compile_options($<$<COMPILE_LANGUAGE:ASM_MASM>:/safeseh>)
endif()

find_package(SwiftSwiftDirectRuntime)

# LNK4049: symbol 'symbol' defined in 'filename.obj' is imported
# LNK4286: symbol 'symbol' defined in 'filename_1.obj' is imported by 'filename_2.obj'
# LNK4217: symbol 'symbol' defined in 'filename_1.obj' is imported by 'filename_2.obj' in function 'function'
#
# We cannot selectively filter the linker warnings as we do not use the MSVC
# frontned and `clang-cl` (and `clang`) currently do not support `/WX:nnnn`. As
# a compromise, treat all linker warnings as errors.
add_link_options($<$<PLATFORM_ID:Windows>:LINKER:/WX>)
# Ensure all symbols are fully resolved on Linux
add_link_options($<$<PLATFORM_ID:Linux>:LINKER:-z,defs>)

add_library(swiftRuntime
  Mangling.swift)

# Optionally add backtracing sources
if(${PROJECT_NAME}_ENABLE_BACKTRACING)
  target_sources(swiftRuntime PRIVATE
    Address.swift
    Backtrace.swift
    Backtrace+Codable.swift
    BacktraceFormatter.swift
    BacktraceJSONFormatter.swift
    Base64.swift
    ByteSwapping.swift
    CachingMemoryReader.swift
    CompactBacktrace.swift
    CompactImageMap.swift
    Compression.swift
    Context.swift
    CoreSymbolication.swift
    CrashLog.swift
    CrashLogCapture.swift
    Dwarf.swift
    EightByteBuffer.swift
    Elf.swift
    ElfImageCache.swift
    FramePointerUnwinder.swift
    Image.swift
    ImageMap.swift
    ImageMap+Darwin.swift
    ImageMap+Linux.swift
    ImageMap+Win32.swift
    ImageSource.swift
    Libc.swift
    LimitSequence.swift
    MemoryReader.swift
    OSReleaseScanner.swift
    PeCoff.swift
    PeImageCache.swift
    ProcMapsScanner.swift
    Registers.swift
    Runtime.swift
    RichFrame.swift
    SymbolicatedBacktrace.swift
    Utils.swift
    Win32Unwinder.swift
  )

  # Add the correct assembly language files
  if(WIN32)
    # On Windows, the assembly syntax differs depending on architecture in
    # such a way that we can't write a single `.asm` file that supports all
    # the different architectures we support.
    string(TOUPPER "${CMAKE_SYSTEM_PROCESSOR}" processor)
    if(processor STREQUAL "AMD64")
      target_sources(swiftRuntime PRIVATE
        get-cpu-context-x86_64.asm)
    elseif(processor STREQUAL "X86")
      target_sources(swiftRuntime PRIVATE
        get-cpu-context-i686.asm)
    elseif(processor STREQUAL "ARM64")
      target_sources(swiftRuntime PRIVATE
        get-cpu-context-aarch64.asm)
    else()
      message(FATAL_ERROR "You need to implement assembly code for ${processor}")
    endif()
  else()
    # Other platforms can use the shared `.S` file.
    target_sources(swiftRuntime
      get-cpu-context.S)
  endif()

endif()

set_target_properties(swiftRuntime PROPERTIES
  Swift_MODULE_NAME Runtime)
target_compile_definitions(swiftRuntime PRIVATE
  SWIFT_ASM_AVAILABLE)
target_compile_options(swiftRuntime PUBLIC
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc -fmodule-map-file=${CMAKE_CURRENT_SOURCE_DIR}/modules/module.modulemap>")
# FIXME: We should split out the parts that are needed by the runtime to avoid
# pulling in headers from the compiler.
target_include_directories(swiftRuntime PRIVATE
  "${${PROJECT_NAME}_SWIFTC_SOURCE_DIR}/include")
# FIXME: Why is this not implicitly in the interface flags?
target_include_directories(swiftRuntime INTERFACE
  "$<$<COMPILE_LANGUAGE:Swift>:$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR}>>")
target_link_libraries(swiftRuntime PUBLIC
  swiftShims
  swiftCore
  swift_Concurrency
  swiftCxxStdlib
  swiftCxx
  $<$<PLATFORM_ID:Android>:swiftAndroid>
  $<$<PLATFORM_ID:Darwin>:swiftDarwin>
  $<$<PLATFORM_ID:Linux>:swiftGlibc>
  $<$<PLATFORM_ID:Windows>:swiftWinSDK>
  $<$<BOOL:${SwiftSwiftDirectRuntime_FOUND}>:swiftSwiftDirectRuntime>)

install(TARGETS swiftRuntime
  EXPORT SwiftRuntimeTargets
  COMPONENT ${PROJECT_NAME}_runtime
  ARCHIVE DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftRuntime)
install_swift_interface(swiftRuntime)

# Configure plist creation for Darwin platforms.
generate_plist(swiftRuntime "${CMAKE_PROJECT_VERSION}" swiftRuntime)
embed_manifest(swiftRuntime)

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/SwiftRuntime.cmake" OPTIONAL)

# Inter-project install info
export(EXPORT SwiftRuntimeTargets
  FILE "cmake/SwiftRuntime/SwiftRuntimeTargets.cmake")
install(EXPORT SwiftRuntimeTargets
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftRuntime"
  FILE "SwiftRuntimeTargets.cmake"
  COMPONENT SwiftRuntimeCMake)
include(CMakePackageConfigHelpers)
configure_package_config_file("${CMAKE_CURRENT_SOURCE_DIR}/cmake/interface/SwiftRuntimeConfig.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftRuntime/SwiftRuntimeConfig.cmake"
  INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftRuntime")
write_basic_package_version_file("${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftRuntime/SwiftRuntimeConfigVersion.cmake"
  VERSION "${PROJECT_VERSION}"
  COMPATIBILITY ExactVersion)
install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftRuntime/SwiftRuntimeConfig.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftRuntime/SwiftRuntimeConfigVersion.cmake"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftRuntime")
