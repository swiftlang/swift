if(("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "${SWIFT_PRIMARY_VARIANT_SDK}") AND
   ("${SWIFT_HOST_VARIANT_ARCH}" STREQUAL "${SWIFT_PRIMARY_VARIANT_ARCH}"))
  if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
    add_swift_unittest(SwiftReflectionTests
      TypeRef.cpp)
    # Link the host library rather than the stdlib's. It's built with the same
    # flags as this test, including sanitizers. The stdlib's copy is never
    # sanitized, which produces false container-overflow reports under ASan.
    target_link_libraries(SwiftReflectionTests
      PRIVATE
      swiftRemoteInspection)
  endif()
endif()
