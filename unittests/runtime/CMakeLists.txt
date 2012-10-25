add_swift_unittest(RuntimeTests
  Metadata.cpp
  )

find_library(FOUNDATION_LIBRARY Foundation)

target_link_libraries(RuntimeTests
  swift_runtime
  ${FOUNDATION_LIBRARY}
  )
