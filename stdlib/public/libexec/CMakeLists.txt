# Make this macOS only for now.
if(OSX IN_LIST SWIFT_SDKS)
add_subdirectory(swift-backtrace)
endif()
