
if (SWIFT_INCLUDE_TESTS)
  foreach(sdk ${SWIFT_SDKS})
  
  set(module_depends)
  set(glibc_sdks LINUX FREEBSD CYGWIN HAIKU)
  
  if(${sdk} IN_LIST SWIFT_APPLE_PLATFORMS)
    list(APPEND module_depends Darwin Foundation)
  elseif(${sdk} IN_LIST glibc_sdks)
    list(APPEND module_depends Glibc)
  elseif(${sdk} STREQUAL WINDOWS)
    list(APPEND module_depends MSVCRT WinSDK)
  endif()

  add_swift_target_library_single_sdk(swiftSwiftReflectionTest ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
    SwiftReflectionTest.swift
    SDK ${sdk}
    SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    SWIFT_COMPILE_FLAGS_LINUX -Xcc -D_GNU_SOURCE
    SWIFT_MODULE_DEPENDS ${module_depends}
    INSTALL_IN_COMPONENT stdlib-experimental
    DARWIN_INSTALL_NAME_DIR "${SWIFT_DARWIN_STDLIB_PRIVATE_INSTALL_NAME_DIR}")

  foreach(ARCH ${SWIFT_SDK_${sdk}_ARCHITECTURES})
    set(VARIANT_SUFFIX "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${ARCH}")
    add_dependencies("swiftSwiftReflectionTest${VARIANT_SUFFIX}"
      "swift-reflection-test${VARIANT_SUFFIX}")
  endforeach()

  endforeach()
endif()
