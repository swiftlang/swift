if(("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "${SWIFT_PRIMARY_VARIANT_SDK}") AND
   ("${SWIFT_HOST_VARIANT_ARCH}" STREQUAL "${SWIFT_PRIMARY_VARIANT_ARCH}"))

  if(SWIFT_BUILD_STATIC_STDLIB AND "${SWIFT_HOST_VARIANT_SDK}" STREQUAL "LINUX")
    set(swift_runtime_test_extra_sources
      "${CMAKE_CURRENT_SOURCE_DIR}/../../stdlib/public/runtime/ImageInspectionELF.cpp")
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
  elseif(SWIFT_HOST_VARIANT STREQUAL "linux")
    list(APPEND PLATFORM_TARGET_LINK_LIBRARIES "atomic")
  elseif(SWIFT_HOST_VARIANT STREQUAL "freebsd")
    find_library(EXECINFO_LIBRARY execinfo)
    list(APPEND PLATFORM_TARGET_LINK_LIBRARIES
      ${EXECINFO_LIBRARY}
      )
  endif()

  add_swift_unittest(SwiftRuntimeTests
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
    ${swift_runtime_test_extra_sources}
    )

  # FIXME: cross-compile for all variants.
  target_link_libraries(SwiftRuntimeTests
    swiftCore${SWIFT_PRIMARY_VARIANT_SUFFIX}
    ${PLATFORM_TARGET_LINK_LIBRARIES}
    )
endif()

