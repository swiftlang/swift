
set(SWIFT_STDLIB_BUILT_STANDALONE FALSE)
if("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_CURRENT_SOURCE_DIR}")
  set(SWIFT_STDLIB_BUILT_STANDALONE TRUE)
endif()

if (SWIFT_STDLIB_BUILT_STANDALONE)
  cmake_minimum_required(VERSION 2.8.12)

  if(POLICY CMP0051)
    cmake_policy(SET CMP0051 NEW)
  endif()

  if(POLICY CMP0054)
    cmake_policy(SET CMP0054 NEW)
  endif()

  # Add path for custom CMake modules.
  list(APPEND CMAKE_MODULE_PATH
    "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/modules")

  include("${SWIFT_STDLIB_GLOBAL_CACHE_PATH}")

  set(SWIFT_PRIMARY_VARIANT_SDK ${SWIFT_STDLIB_PRIMARY_VARIANT_SDK})
  set(SWIFT_PRIMARY_VARIANT_ARCH ${SWIFT_STDLIB_PRIMARY_VARIANT_ARCH})
  set(SWIFT_PRIMARY_VARIANT_SUFFIX
    "-${SWIFT_SDK_${SWIFT_PRIMARY_VARIANT_SDK}_LIB_SUBDIR}-${SWIFT_PRIMARY_VARIANT_ARCH}")

  include(SwiftUtils)

  #
  # Assume a new enough ar to generate the index at construction time. This avoids
  # having to invoke ranlib as a secondary command.
  #

  set(CMAKE_C_ARCHIVE_CREATE "<CMAKE_AR> crs <TARGET> <LINK_FLAGS> <OBJECTS>")
  set(CMAKE_C_ARCHIVE_APPEND "<CMAKE_AR> qs <TARGET> <LINK_FLAGS> <OBJECTS>")
  set(CMAKE_C_ARCHIVE_FINISH "")

  set(CMAKE_CXX_ARCHIVE_CREATE "<CMAKE_AR> crs <TARGET> <LINK_FLAGS> <OBJECTS>")
  set(CMAKE_CXX_ARCHIVE_APPEND "<CMAKE_AR> qs <TARGET> <LINK_FLAGS> <OBJECTS>")
  set(CMAKE_CXX_ARCHIVE_FINISH "")

  #
  # Include CMake modules
  #

  include(CheckCXXSourceRuns)
  include(CMakeParseArguments)
  include(SwiftHandleGybSources)
  include(SwiftSetIfArchBitness)
  include(SwiftSource)
  include(AddSwift)
  include(SwiftConfigureSDK)
  include(SwiftComponents)
  include(SwiftList)

  swift_configure_install_components("${SWIFT_INSTALL_COMPONENTS}")

  # Reset CMAKE_SYSTEM_PROCESSOR if not cross-compiling.
  # CMake refuses to use `uname -m` on OS X
  # http://public.kitware.com/Bug/view.php?id=10326
  if(NOT CMAKE_CROSSCOMPILING AND CMAKE_SYSTEM_PROCESSOR STREQUAL "i386")
    execute_process(
      COMMAND "uname" "-m"
      OUTPUT_VARIABLE CMAKE_SYSTEM_PROCESSOR
      OUTPUT_STRIP_TRAILING_WHITESPACE)
  endif()

  include(SwiftSharedCMakeConfig)

  # Support building Swift as a standalone project, using LLVM as an
  # external library.
  swift_common_standalone_build_config(SWIFT SWIFT_CROSS_COMPILING)

  # This setting causes all CMakeLists.txt to automatically have
  # ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CURRENT_SOURCE_DIR} as an
  # include_directories path. This is done for developer
  # convenience. Additionally, LLVM/Clang build with this option enabled, so we
  # should match them unless it is removed from LLVM/Clang as well.
  #
  # *NOTE* Even though these directories are added to the include path for a
  # specific CMakeLists.txt, these include paths are not propagated down to
  # subdirectories.
  set(CMAKE_INCLUDE_CURRENT_DIR ON)

  # We'll need this once we have generated headers
  include_directories(BEFORE
    ${CMAKE_CURRENT_BINARY_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    )

  # Xcode: use libc++ and c++11 using proper build settings.
  if(XCODE)
    swift_common_xcode_cxx_config()
  endif()

  include(SwiftCheckCXXNativeRegex)
  check_cxx_native_regex(SWIFT_HAVE_WORKING_STD_REGEX)

  set(SWIFT_STDLIB_LIBRARY_BUILD_TYPES)
  if(SWIFT_BUILD_DYNAMIC_STDLIB)
    list(APPEND SWIFT_STDLIB_LIBRARY_BUILD_TYPES SHARED)
  endif()
  if(SWIFT_BUILD_STATIC_STDLIB)
    list(APPEND SWIFT_STDLIB_LIBRARY_BUILD_TYPES STATIC)
  endif()

  add_custom_target(swift-stdlib-all)
  add_custom_target("swift-stdlib-${SWIFT_SDK_${SWIFT_PRIMARY_VARIANT_SDK}_LIB_SUBDIR}")
  add_custom_target("swift-test-stdlib-${SWIFT_SDK_${SWIFT_PRIMARY_VARIANT_SDK}_LIB_SUBDIR}")
  set(VARIANT_SUFFIX "-${SWIFT_SDK_${SWIFT_PRIMARY_VARIANT_SDK}_LIB_SUBDIR}-${SWIFT_PRIMARY_VARIANT_ARCH}")
  add_custom_target("swift-stdlib${VARIANT_SUFFIX}")
  add_custom_target("swift-test-stdlib${VARIANT_SUFFIX}")
  add_dependencies(swift-stdlib-all "swift-stdlib${VARIANT_SUFFIX}")
  add_dependencies("swift-stdlib-${SWIFT_SDK_${SWIFT_PRIMARY_VARIANT_SDK}_LIB_SUBDIR}"
    "swift-stdlib${VARIANT_SUFFIX}")
  add_dependencies("swift-test-stdlib-${SWIFT_SDK_${SWIFT_PRIMARY_VARIANT_SDK}_LIB_SUBDIR}"
    "swift-test-stdlib${VARIANT_SUFFIX}")

  add_custom_target(swift-stdlib
    DEPENDS "swift-stdlib${SWIFT_PRIMARY_VARIANT_SUFFIX}")
  add_custom_target(swift-test-stdlib ALL
    DEPENDS "swift-test-stdlib${SWIFT_PRIMARY_VARIANT_SUFFIX}")

  if(SWIFT_STDLIB_ENABLE_RESILIENCE)
    set(STDLIB_SIL_SERIALIZE_ALL)
  else()
    if(SWIFT_STDLIB_SIL_SERIALIZE_ALL)
      set(STDLIB_SIL_SERIALIZE_ALL "-Xfrontend" "-sil-serialize-all")
    else()
      set(STDLIB_SIL_SERIALIZE_ALL)
    endif()
  endif()

  include_directories(BEFORE
    ${SWIFT_BINARY_DIR}/include
    ${SWIFT_SOURCE_DIR}/include
    )
  # lipo is used to create universal binaries.
  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    include(SwiftDarwin)
    find_toolchain_tool(LIPO "${SWIFT_DARWIN_XCRUN_TOOLCHAIN}" lipo)
  endif()
endif()

add_subdirectory(public)
add_subdirectory(internal)
add_subdirectory(private)
