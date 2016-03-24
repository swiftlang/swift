add_swift_library(swiftOption
  Options.cpp
  SanitizerOptions.cpp
  DEPENDS SwiftOptions
  LINK_LIBRARIES swiftBasic
  FILE_DEPENDS SwiftOptions)

