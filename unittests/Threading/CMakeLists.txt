add_swift_unittest(SwiftThreadingTests
  Mutex.cpp
  Once.cpp
  LinuxUlock.cpp
  Fatal.cpp
  )

target_link_libraries(SwiftThreadingTests
  PRIVATE
  swiftThreading${SWIFT_PRIMARY_VARIANT_SUFFIX}
  swiftCore${SWIFT_PRIMARY_VARIANT_SUFFIX}
  )

if(CMAKE_SYSTEM_NAME STREQUAL Windows)
  target_link_libraries(SwiftThreadingTests PRIVATE Synchronization)
endif()
