add_swift_host_library(swiftOption STATIC
  Options.cpp
  SanitizerOptions.cpp)
add_dependencies(swiftOption
  SwiftOptions)
target_link_libraries(swiftOption PRIVATE
  swiftBasic)

