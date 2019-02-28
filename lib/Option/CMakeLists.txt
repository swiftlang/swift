add_swift_host_library(swiftOption STATIC
  Options.cpp
  SanitizerOptions.cpp
  DEPENDS SwiftOptions
  FILE_DEPENDS SwiftOptions)
target_link_libraries(swiftOption PRIVATE
  swiftBasic)

