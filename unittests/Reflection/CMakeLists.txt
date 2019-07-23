if(("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "${SWIFT_PRIMARY_VARIANT_SDK}") AND
   ("${SWIFT_HOST_VARIANT_ARCH}" STREQUAL "${SWIFT_PRIMARY_VARIANT_ARCH}"))
  if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
    add_swift_unittest(SwiftReflectionTests
      TypeRef.cpp)
    target_link_libraries(SwiftReflectionTests
      PRIVATE
      swiftReflection${SWIFT_PRIMARY_VARIANT_SUFFIX})
  endif()
endif()
