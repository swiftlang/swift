add_swift_host_library(swiftOption STATIC
  Options.cpp
  SanitizerOptions.cpp
  FILE_DEPENDS SwiftOptions)
add_dependencies(swiftOption
  SwiftOptions)
target_link_libraries(swiftOption PRIVATE
  swiftBasic)

