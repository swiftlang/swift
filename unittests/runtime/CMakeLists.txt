if(("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "${SWIFT_PRIMARY_VARIANT_SDK}") AND
   ("${SWIFT_HOST_VARIANT_ARCH}" STREQUAL "${SWIFT_PRIMARY_VARIANT_ARCH}"))

  if("${CMAKE_C_COMPILER_ID}" MATCHES "Clang")
    # Do nothing
  elseif(SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER)
    if(NOT "${CMAKE_C_COMPILER_ID}" MATCHES "Clang")
      message(FATAL_ERROR "Building the swift runtime is not supported with ${CMAKE_C_COMPILER_ID}. Use the just-built clang instead.")
    endif()
  else()
    message(WARNING "Building the swift runtime using the host compiler, and not the just-built clang.")

    # If we use Clang-cl or MSVC, CMake provides default compiler and linker flags that are incompatible
    # with the frontend of Clang or Clang++.
    if(SWIFT_COMPILER_IS_MSVC_LIKE)
      set(CMAKE_CXX_COMPILER "${SWIFT_NATIVE_LLVM_TOOLS_PATH}/clang-cl")
      set(CMAKE_C_COMPILER "${SWIFT_NATIVE_LLVM_TOOLS_PATH}/clang-cl")
    else()
      set(CMAKE_CXX_COMPILER "${SWIFT_NATIVE_LLVM_TOOLS_PATH}/clang++")
      set(CMAKE_C_COMPILER "${SWIFT_NATIVE_LLVM_TOOLS_PATH}/clang")
    endif()

    if(CMAKE_C_COMPILER_LAUNCHER MATCHES ".*distcc")
      set(CMAKE_C_COMPILER_LAUNCHER "")
    endif()
    if(CMAKE_CXX_COMPILER_LAUNCHER MATCHES ".*distcc")
      set(CMAKE_CXX_COMPILER_LAUNCHER "")
    endif()
  endif()

  set(swift_runtime_test_extra_libraries)
  if(SWIFT_BUILD_STATIC_STDLIB AND "${SWIFT_HOST_VARIANT_SDK}" STREQUAL "LINUX")
    list(APPEND swift_runtime_test_extra_libraries 
      $<TARGET_FILE:swiftImageInspectionShared-${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}-${SWIFT_HOST_VARIANT_ARCH}>)
  endif()

  add_subdirectory(LongTests)

  set(PLATFORM_SOURCES)
  set(PLATFORM_TARGET_LINK_LIBRARIES)
  if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
    find_library(FOUNDATION_LIBRARY Foundation)
    list(APPEND PLATFORM_SOURCES
      weak.mm
      Refcounting.mm
      )
    # We need to link swiftCore on Darwin because the runtime still relies on
    # some stdlib hooks to implement SwiftObject.
    list(APPEND PLATFORM_TARGET_LINK_LIBRARIES
      ${FOUNDATION_LIBRARY}
      swiftStdlibUnittest${SWIFT_PRIMARY_VARIANT_SUFFIX}
      )
  elseif(SWIFT_HOST_VARIANT STREQUAL "freebsd")
    find_library(EXECINFO_LIBRARY execinfo)
    list(APPEND PLATFORM_TARGET_LINK_LIBRARIES
      ${EXECINFO_LIBRARY}
      )
  elseif(SWIFT_HOST_VARIANT STREQUAL windows)
    list(APPEND PLATFORM_TARGET_LINK_LIBRARIES DbgHelp)
  endif()

  add_swift_unittest(SwiftRuntimeTests
    Array.cpp
    CompatibilityOverride.cpp
    Concurrent.cpp
    Metadata.cpp
    Mutex.cpp
    Enum.cpp
    Refcounting.cpp
    Stdlib.cpp
    ${PLATFORM_SOURCES}

    # The runtime tests link to internal runtime symbols, which aren't exported
    # from the swiftCore dylib, so we need to link to both the runtime archive
    # and the stdlib.
    $<TARGET_OBJECTS:swiftRuntime${SWIFT_PRIMARY_VARIANT_SUFFIX}>
    $<TARGET_OBJECTS:swiftLLVMSupport${SWIFT_PRIMARY_VARIANT_SUFFIX}>
    $<TARGET_OBJECTS:swiftDemangling${SWIFT_PRIMARY_VARIANT_SUFFIX}>
    )

  # The local stdlib implementation provides definitions of the swiftCore
  # interfaes to avoid pulling in swiftCore itself.  Build the SwiftRuntimeTests
  # with swiftCore_EXPORTS to permit exporting the stdlib interfaces.
  target_compile_definitions(SwiftRuntimeTests
                             PRIVATE
                               swiftCore_EXPORTS
                               SWIFT_INLINE_NAMESPACE=__runtime)

  target_include_directories(SwiftRuntimeTests BEFORE PRIVATE
    ${SWIFT_SOURCE_DIR}/stdlib/include)

  # FIXME: cross-compile for all variants.
  target_link_libraries(SwiftRuntimeTests
    PRIVATE
    swiftCore${SWIFT_PRIMARY_VARIANT_SUFFIX}
    ${PLATFORM_TARGET_LINK_LIBRARIES}
    ${swift_runtime_test_extra_libraries}
    )
endif()

