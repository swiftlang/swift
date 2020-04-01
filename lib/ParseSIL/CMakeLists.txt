add_swift_host_library(swiftParseSIL STATIC
  ParseSIL.cpp)
target_link_libraries(swiftParseSIL PRIVATE
  swiftSema
  swiftSIL)

