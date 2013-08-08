add_swift_unittest(SwiftIDETests
  CodeCompletionToken.cpp
  )
target_link_libraries(SwiftIDETests
  swiftIDE
)
llvm_config(SwiftIDETests bitreader bitwriter support ${LLVM_TARGETS_TO_BUILD})
