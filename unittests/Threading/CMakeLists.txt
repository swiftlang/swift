if(("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "${SWIFT_PRIMARY_VARIANT_SDK}") AND
   ("${SWIFT_HOST_VARIANT_ARCH}" STREQUAL "${SWIFT_PRIMARY_VARIANT_ARCH}"))
  add_swift_unittest(SwiftThreadingTests IS_TARGET_TEST
    Mutex.cpp
    ConditionVariable.cpp
    Once.cpp
    LinuxUlock.cpp
    Fatal.cpp
    )

  # Use the Swift SDK's runtime when this tree doesn't build one.
  if(SWIFT_TEST_SDK AND NOT TARGET swiftCore${SWIFT_PRIMARY_VARIANT_SUFFIX})
    set(swift_core "${SWIFT_TEST_SDK}/usr/lib/swift/${SWIFT_SDK_${SWIFT_PRIMARY_VARIANT_SDK}_LIB_SUBDIR}/${SWIFT_PRIMARY_VARIANT_ARCH}/${CMAKE_IMPORT_LIBRARY_PREFIX}swiftCore${CMAKE_IMPORT_LIBRARY_SUFFIX}")
  else()
    set(swift_core swiftCore${SWIFT_PRIMARY_VARIANT_SUFFIX})
  endif()

  target_link_libraries(SwiftThreadingTests
    PRIVATE
    swiftThreading${SWIFT_PRIMARY_VARIANT_SUFFIX}
    ${swift_core}
    )

  if(SWIFT_HOST_VARIANT STREQUAL "windows")
    target_link_libraries(SwiftThreadingTests PRIVATE Synchronization)
  endif()
endif()
