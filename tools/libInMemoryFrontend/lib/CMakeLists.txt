add_swift_host_library(libInMemoryFrontend STATIC
  InMemoryFrontend.cpp
)

target_link_libraries(libInMemoryFrontend PRIVATE
  swiftFrontend
  swiftSerialization
  swiftSIL
)
