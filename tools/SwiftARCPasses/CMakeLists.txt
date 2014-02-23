set(MODULE TRUE)

add_swift_library(SwiftARCPasses
  SwiftARCPasses.cpp
  DEPENDS swiftOptimizeARC
  COMPONENT_DEPENDS support
)
