# Early swift-driver is built adjacent to the compiler (swift build dir).
# Compute this unconditionally. The RPATH and link-directory settings below
# consume it even when SwiftDriver_DIR and TSC_DIR are supplied directly, and
# an empty value there expands to bogus absolute root paths.
if(SWIFT_EARLY_SWIFT_DRIVER_BUILD)
  get_filename_component(driver_package_dir "${SWIFT_EARLY_SWIFT_DRIVER_BUILD}" DIRECTORY)
else()
  if(CMAKE_BUILD_TYPE STREQUAL "Release" OR CMAKE_BUILD_TYPE STREQUAL "RelWithDebInfo")
    set(driver_package_configuration_dir "release")
  else()
    set(driver_package_configuration_dir "debug")
  endif()
  set(driver_package_dir
      "${CMAKE_BINARY_DIR}/../earlyswiftdriver-${SWIFT_HOST_VARIANT}-${SWIFT_HOST_VARIANT_ARCH}/${driver_package_configuration_dir}")
endif()

if(NOT SwiftDriver_DIR)
  set(SwiftDriver_DIR "${driver_package_dir}/cmake/modules/")
endif()

if(NOT TSC_DIR)
  set(TSC_DIR "${driver_package_dir}/dependencies/swift-tools-support-core/cmake/modules/")
  if(NOT EXISTS "${TSC_DIR}/TSCConfig.cmake")
    if(EXISTS "${driver_package_dir}/_deps/toolssupportcore-build/cmake/modules/TSCConfig.cmake")
      set(TSC_DIR "${driver_package_dir}/_deps/toolssupportcore-build/cmake/modules/")
    elseif(EXISTS "${driver_package_dir}/cmake/modules/TSCConfig.cmake")
      set(TSC_DIR "${driver_package_dir}/cmake/modules/")
    endif()
  endif()
endif()

find_package(TSC CONFIG)
find_package(SwiftDriver CONFIG)

if(TARGET SwiftDriver AND NOT TARGET TSCBasic)
  if(EXISTS "${driver_package_dir}/lib/libTSCBasic.lib")
    add_library(TSCBasic STATIC IMPORTED)
    set_target_properties(TSCBasic PROPERTIES
      IMPORTED_LOCATION "${driver_package_dir}/lib/libTSCBasic.lib"
    )
  elseif(EXISTS "${driver_package_dir}/lib/TSCBasic.lib")
    add_library(TSCBasic STATIC IMPORTED)
    set_target_properties(TSCBasic PROPERTIES
      IMPORTED_LOCATION "${driver_package_dir}/lib/TSCBasic.lib"
    )
  endif()
endif()

if(TARGET SwiftDriver AND NOT TARGET TSCUtility)
  if(EXISTS "${driver_package_dir}/lib/libTSCUtility.lib")
    add_library(TSCUtility STATIC IMPORTED)
    set_target_properties(TSCUtility PROPERTIES
      IMPORTED_LOCATION "${driver_package_dir}/lib/libTSCUtility.lib"
    )
  elseif(EXISTS "${driver_package_dir}/lib/TSCUtility.lib")
    add_library(TSCUtility STATIC IMPORTED)
    set_target_properties(TSCUtility PROPERTIES
      IMPORTED_LOCATION "${driver_package_dir}/lib/TSCUtility.lib"
    )
  endif()
endif()


add_swift_host_tool(swift-ide-test
  swift-ide-test.cpp
  ModuleAPIDiff.cpp
  XMLValidator.cpp
  SWIFT_COMPONENT tools
  THINLTO_LD64_ADD_FLTO_CODEGEN_ONLY
  HAS_SWIFT_MODULES
)

if(SWIFT_BUILD_COMPILER_DYLIB)
  target_link_libraries(swift-ide-test
                        PRIVATE
                          SwiftCompilerShared)
else()
  target_link_libraries(swift-ide-test
                        PRIVATE
                          swiftAST
                          swiftDriver
                          swiftFrontend
                          swiftIDE
                          swiftIDETool
                          swiftCompilerModules)
endif()

set_target_properties(swift-ide-test PROPERTIES
  BUILD_WITH_INSTALL_RPATH NO
)
# BUILD_WITH_INSTALL_RPATH NO discards the INSTALL_RPATH entries that
# _add_swift_runtime_link_flags also relies on in the build tree, so restore the
# relocatable path to the host swift-syntax libraries.
set_property(
  TARGET swift-ide-test
  APPEND PROPERTY BUILD_RPATH "@executable_path/../lib/swift/host/compiler"
)
set_property(
  TARGET swift-ide-test
  APPEND PROPERTY BUILD_RPATH "${driver_package_dir}/lib/"
)
set_property(
  TARGET swift-ide-test
  APPEND PROPERTY BUILD_RPATH "${driver_package_dir}/dependencies/swift-tools-support-core/lib/"
)

target_link_directories(swift-ide-test PRIVATE
  "${driver_package_dir}/lib"
  "${driver_package_dir}/dependencies/swift-tools-support-core/lib"
)

if(SWIFT_HOST_VARIANT_SDK STREQUAL "WINDOWS")
  target_link_directories(swift-ide-test PRIVATE
    "${driver_package_dir}/bin"
    "${driver_package_dir}/dependencies/swift-tools-support-core/bin"
    "${SWIFT_PATH_TO_SWIFT_SDK}/usr/lib/swift/${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}"
    "${SWIFT_PATH_TO_SWIFT_SDK}/usr/lib/swift/${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}/${SWIFT_HOST_VARIANT_ARCH}"
    "${SWIFT_PATH_TO_SWIFT_SDK}/usr/lib/swift_static/${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}"
    "${SWIFT_PATH_TO_SWIFT_SDK}/usr/lib/swift_static/${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}/${SWIFT_HOST_VARIANT_ARCH}"
  )
endif()

target_link_libraries(swift-ide-test
                      PRIVATE
                        swiftAST
                        swiftDriver
                        SwiftDriver
                        swiftFrontend
                        swiftIDE
                        swiftIDETool
                        swiftCompilerModules
                        libSwiftScanDependencies)
target_sources(swift-ide-test
  PRIVATE
  $<TARGET_OBJECTS:swiftScanImpl>)

# The early swift-driver is optional: `--skip-early-swift-driver`, and a host
# toolchain with no `swiftc`, both leave it unbuilt. On Windows, the early swift-driver
# is built statically with `-static-stdlib` by build.ps1 and cannot be linked into
# C++ host tools. Link it only when find_package actually produced the target and
# the host platform supports it, and let swift-ide-test fall back to exercising the
# legacy driver alone.
if(TARGET SwiftDriver AND NOT SWIFT_HOST_VARIANT_SDK STREQUAL "WINDOWS")
  set_property(
    TARGET swift-ide-test
    APPEND PROPERTY BUILD_RPATH "${driver_package_dir}/lib/"
  )
  set_property(
    TARGET swift-ide-test
    APPEND PROPERTY BUILD_RPATH "${driver_package_dir}/dependencies/swift-tools-support-core/lib/"
  )

  target_link_directories(swift-ide-test PRIVATE
    "${driver_package_dir}/lib"
    "${driver_package_dir}/dependencies/swift-tools-support-core/lib"
  )

  if(SWIFT_HOST_VARIANT_SDK STREQUAL "WINDOWS")
    target_link_directories(swift-ide-test PRIVATE
      "${driver_package_dir}/bin"
      "${driver_package_dir}/dependencies/swift-tools-support-core/bin"
      "${SWIFT_PATH_TO_SWIFT_SDK}/usr/lib/swift/${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}"
      "${SWIFT_PATH_TO_SWIFT_SDK}/usr/lib/swift/${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}/${SWIFT_HOST_VARIANT_ARCH}"
      "${SWIFT_PATH_TO_SWIFT_SDK}/usr/lib/swift_static/${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}"
      "${SWIFT_PATH_TO_SWIFT_SDK}/usr/lib/swift_static/${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}/${SWIFT_HOST_VARIANT_ARCH}"
    )
  endif()

  target_link_libraries(swift-ide-test PRIVATE SwiftDriver)
  target_compile_definitions(swift-ide-test PRIVATE SWIFT_IDE_TEST_HAS_SWIFT_DRIVER=1)

  # In the build tree the driver dylibs come from the early swift-driver, via
  # the BUILD_RPATH entries above. An installed toolchain instead gets them from
  # the (non-early) SwiftDriver build product, which puts libSwiftDriver,
  # libSwiftOptions, libTSCBasic and libTSCUtility in <prefix>/lib/swift/<platform>.
  if(SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_DARWIN_PLATFORMS)
    set_property(
      TARGET swift-ide-test
      APPEND PROPERTY INSTALL_RPATH
        "@executable_path/../lib/swift/${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}"
    )
  else()
    set_property(
      TARGET swift-ide-test
      APPEND PROPERTY INSTALL_RPATH
        "$ORIGIN/../lib/swift/${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}"
    )
  endif()

  # The lit suite gates its new-driver checks on this.
  set(SWIFT_IDE_TEST_HAS_SWIFT_DRIVER TRUE CACHE INTERNAL
      "swift-ide-test links the early swift-driver")
else()
  message(STATUS "swift-ide-test: early swift-driver not found, "
                 "-test-createCompilerInvocation will not exercise the new driver")
  set(SWIFT_IDE_TEST_HAS_SWIFT_DRIVER FALSE CACHE INTERNAL
      "swift-ide-test links the early swift-driver")
endif()

# If libxml2 is available, make it available for swift-ide-test.
if(SWIFT_HAVE_LIBXML2)
  target_link_libraries(swift-ide-test PRIVATE LibXml2::LibXml2)
  target_compile_definitions(swift-ide-test PRIVATE SWIFT_HAVE_LIBXML=1)
endif()

# Create a symlink for swift-api-dump.py in the bin directory
swift_create_post_build_symlink(swift-ide-test
  SOURCE "${SWIFT_SOURCE_DIR}/utils/swift-api-dump.py"
  DESTINATION "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swift-api-dump.py"
  COMMENT "Creating development symlink for swift-api-dump.py.")
