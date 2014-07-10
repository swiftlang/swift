add_swift_library(swiftOption
  Options.cpp
  DEPENDS swiftBasic)

add_dependencies(swiftOption SwiftOptions)
