if(("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "${SWIFT_PRIMARY_VARIANT_SDK}") AND
   ("${SWIFT_HOST_VARIANT_ARCH}" STREQUAL "${SWIFT_PRIMARY_VARIANT_ARCH}"))

  set(PLATFORM_SOURCES)
  set(PLATFORM_TARGET_LINK_LIBRARIES)
  if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
    find_library(FOUNDATION_LIBRARY Foundation)
    list(APPEND PLATFORM_SOURCES
      weak.mm
      )
    list(APPEND PLATFORM_TARGET_LINK_LIBRARIES
      ${FOUNDATION_LIBRARY}
      swiftStdlibUnittest${SWIFT_PRIMARY_VARIANT_SUFFIX}
      )
  endif()

  add_swift_unittest(SwiftRuntimeTests
    Metadata.cpp
    Enum.cpp
    Refcounting.cpp
    ${PLATFORM_SOURCES}
    )

  # FIXME: cross-compile for all variants.
  target_link_libraries(SwiftRuntimeTests
    swiftCore${SWIFT_PRIMARY_VARIANT_SUFFIX}
    ${PLATFORM_TARGET_LINK_LIBRARIES}
    )
endif()

