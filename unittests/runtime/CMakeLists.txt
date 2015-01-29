if(("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "${SWIFT_PRIMARY_VARIANT_SDK}") AND
   ("${SWIFT_HOST_VARIANT_ARCH}" STREQUAL "${SWIFT_PRIMARY_VARIANT_ARCH}"))

  add_swift_unittest(SwiftRuntimeTests
    Metadata.cpp
    Enum.cpp
    Refcounting.cpp
    weak.mm
    )
  target_link_libraries(SwiftRuntimeTests swiftStdlibUnittest${SWIFT_PRIMARY_VARIANT_SUFFIX})

  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    find_library(FOUNDATION_LIBRARY Foundation)
    target_link_libraries(SwiftRuntimeTests ${FOUNDATION_LIBRARY})
  endif()

  # FIXME: cross-compile for all variants.
  target_link_libraries(SwiftRuntimeTests
    swiftCore${SWIFT_PRIMARY_VARIANT_SUFFIX})
endif()

