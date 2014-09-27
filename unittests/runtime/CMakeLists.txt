add_swift_unittest(SwiftRuntimeTests
  Metadata.cpp
  Enum.cpp
  )

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  find_library(FOUNDATION_LIBRARY Foundation)
  target_link_libraries(SwiftRuntimeTests ${FOUNDATION_LIBRARY})
endif()

target_link_libraries(SwiftRuntimeTests
  swiftCore
  )
