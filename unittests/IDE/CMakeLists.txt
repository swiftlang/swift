add_swift_unittest(SwiftIDETests
  CodeCompletionToken.cpp
  Placeholders.cpp
  )
target_link_libraries(SwiftIDETests
  swiftIDE
)
