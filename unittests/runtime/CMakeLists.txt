add_swift_unittest(SwiftRuntimeTests
  Metadata.cpp
  )

find_library(FOUNDATION_LIBRARY Foundation)

target_link_libraries(SwiftRuntimeTests
  curses
  swift_runtime
  ${FOUNDATION_LIBRARY}
  )
