add_swift_unittest(SwiftRuntimeTests
  Metadata.cpp
  Enum.cpp
  )

find_library(FOUNDATION_LIBRARY Foundation)

target_link_libraries(SwiftRuntimeTests
  swift_runtime
  ${FOUNDATION_LIBRARY}
  )
