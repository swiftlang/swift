set(SHARED_LIBRARY ON)
add_swift_library(swiftCoreImage INSTALL
  CoreImage.swift
  DEPENDS swift_stdlib_core swiftObjectiveC)

add_swift_optimization_flags(swiftCoreImage)

# Link against frameworks
target_link_libraries(swiftCoreImage
  "-framework CoreImage")
