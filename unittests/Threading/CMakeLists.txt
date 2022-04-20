add_swift_unittest(SwiftThreadingTests
  Mutex.cpp
  )

target_link_libraries(SwiftThreadingTests
  PRIVATE
  swiftThreading${SWIFT_PRIMARY_VARIANT_SUFFIX}
  swiftCore${SWIFT_PRIMARY_VARIANT_SUFFIX}
  )
