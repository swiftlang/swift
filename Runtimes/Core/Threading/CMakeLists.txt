# TODO: I think we can build the one that actually matters based on the
#       platform we're building, rather than relying just on macro definitions
#       and linking empty objects.
#       https://github.com/swiftlang/swift/issues/77435
add_library(swiftThreading OBJECT
  "${PROJECT_SOURCE_DIR}/../../lib/Threading/C11.cpp"
  "${PROJECT_SOURCE_DIR}/../../lib/Threading/Linux.cpp"
  "${PROJECT_SOURCE_DIR}/../../lib/Threading/Pthreads.cpp"
  "${PROJECT_SOURCE_DIR}/../../lib/Threading/Win32.cpp")
target_link_libraries(swiftThreading PRIVATE swiftShims)
