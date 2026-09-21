if(NOT SWIFT_BUILD_SARIF)
  return()
endif()
if(NOT EXISTS "${SWIFT_PATH_TO_SARIF_SOURCE}")
  message(SEND_ERROR "swift-toolchain-sarif is required to build SARIF diagnostic support. Please run update-checkout or specify SWIFT_PATH_TO_SARIF_SOURCE")
  return()
endif()

# Build swift-toolchain-sarif libraries with FetchContent.
function(includeSARIF)
  set(CMAKE_SYSTEM_PROCESSOR ${SWIFT_HOST_VARIANT_ARCH})
  set(CMAKE_OSX_ARCHITECTURES ${SWIFT_HOST_VARIANT_ARCH})
  set(CMAKE_C_COMPILER_TARGET ${SWIFT_HOST_TRIPLE})
  set(CMAKE_CXX_COMPILER_TARGET ${SWIFT_HOST_TRIPLE})
  set(CMAKE_Swift_COMPILER_TARGET ${SWIFT_HOST_TRIPLE})
  set(BUILD_SHARED_LIBS ON)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${SWIFT_HOST_LIBRARIES_DEST_DIR}/compiler")
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${SWIFT_HOST_LIBRARIES_DEST_DIR}/compiler")
  if(SWIFT_HOST_VARIANT_SDK MATCHES "LINUX|ANDROID|OPENBSD|FREEBSD")
    set(SWIFT_HOST_LIBRARIES_RPATH "$ORIGIN;$ORIGIN/../../${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}")
  endif()

  # Add unique ABI prefix to the SARIF libraries so that compiler libraries can
  # be used from tools that have their own copy as a SwiftPM dependency.
  set(SWIFT_MODULE_ABI_NAME_PREFIX "_Compiler")
  set(SARIF_TARGET_NAMESPACE "_Compiler")

  # swift-toolchain-sarif needs 'OrderedCollections' from swift-collections. We
  # hand it the checkout that update-checkout produced so that configuration
  # does not reach the network.
  if(EXISTS "${SWIFT_PATH_TO_SWIFT_COLLECTIONS_SOURCE}")
    set(_SwiftCollections_SourceDIR "${SWIFT_PATH_TO_SWIFT_COLLECTIONS_SOURCE}")
  endif()

  file(TO_CMAKE_PATH "${SWIFT_PATH_TO_SARIF_SOURCE}" sarif_path)
  FetchContent_Declare(CompilerSARIF SOURCE_DIR "${sarif_path}")
  FetchContent_MakeAvailable(CompilerSARIF)

  # FIXME: Use FetchContent_Declare's EXCLUDE_FROM_ALL after CMake 3.28
  FetchContent_GetProperties(CompilerSARIF BINARY_DIR binary_dir)
  set_property(DIRECTORY "${binary_dir}" PROPERTY EXCLUDE_FROM_ALL TRUE)
endfunction()
includeSARIF()

# 'SARIFMerge' is omitted because merging SARIF logs is a job for downstream
# tooling, not the compiler.
set(compiler_sarif_libs
  _CompilerImmutableJSON
  _CompilerSARIFRecords
  _CompilerSARIF
)

foreach(lib ${compiler_sarif_libs})
  target_compile_options(${lib} PRIVATE "SHELL:-module-link-name ${lib}")
  _add_swift_caching_compile_options(${lib})
endforeach()

# The SARIF libraries record a dependency on swift-collections' libraries.The
# SARIF package co-locates them in the build tree.
set(compiler_sarif_install_libs ${compiler_sarif_libs})
foreach(lib ${SARIF_SWIFT_COLLECTIONS_TARGETS})
  if(TARGET ${lib})
    list(APPEND compiler_sarif_install_libs ${lib})
  endif()
endforeach()

swift_install_in_component(TARGETS ${compiler_sarif_install_libs}
  ARCHIVE DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/host/compiler" COMPONENT compiler-swift-toolchain-sarif-lib
  LIBRARY DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/host/compiler" COMPONENT compiler-swift-toolchain-sarif-lib
  RUNTIME DESTINATION "bin" COMPONENT compiler-swift-toolchain-sarif-lib)
add_dependencies(compiler-swift-toolchain-sarif-lib ${compiler_sarif_install_libs})
