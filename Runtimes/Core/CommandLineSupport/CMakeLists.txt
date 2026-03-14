# TODO(etcwilde) migrate this into subdir subdirectory once the migration is
# completed.
target_sources(swiftCore PRIVATE
  CommandLine.cpp)
target_compile_definitions(swiftCore PUBLIC
  -DSWIFT_STDLIB_HAS_COMMANDLINE)
target_include_directories(swiftCore PRIVATE
  "${SwiftCore_SWIFTC_SOURCE_DIR}/include"
  "${PROJECT_BINARY_DIR}/include")
