# Make this macOS only for now.
if(SWIFT_ENABLE_BACKTRACING)
  add_subdirectory(swift-backtrace)
endif()
