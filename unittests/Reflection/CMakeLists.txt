if(("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "${SWIFT_PRIMARY_VARIANT_SDK}") AND
   ("${SWIFT_HOST_VARIANT_ARCH}" STREQUAL "${SWIFT_PRIMARY_VARIANT_ARCH}"))
  set(PLATFORM_TARGET_LINK_LIBRARIES)

  list(APPEND PLATFORM_TARGET_LINK_LIBRARIES
    swiftReflection${SWIFT_PRIMARY_VARIANT_SUFFIX})

  add_swift_unittest(SwiftReflectionTests
    MetadataSource.cpp)

  target_link_libraries(SwiftReflectionTests
    swiftReflection${SWIFT_PRIMARY_VARIANT_SUFFIX}
    ${PLATFORM_TARGET_LINK_LIBRARIES})
endif()

