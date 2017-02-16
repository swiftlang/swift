add_swift_library(swiftOption STATIC
  Options.cpp
  SanitizerOptions.cpp
  DEPENDS SwiftOptions
  LINK_LIBRARIES swiftBasic
  FILE_DEPENDS SwiftOptions)

