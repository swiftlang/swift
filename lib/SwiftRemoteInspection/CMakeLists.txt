
add_swift_host_library(swiftRemoteInspection STATIC
  ${SWIFT_SOURCE_DIR}/stdlib/public/RemoteInspection/MetadataSource.cpp
  ${SWIFT_SOURCE_DIR}/stdlib/public/RemoteInspection/TypeLowering.cpp
  ${SWIFT_SOURCE_DIR}/stdlib/public/RemoteInspection/TypeRef.cpp
  ${SWIFT_SOURCE_DIR}/stdlib/public/RemoteInspection/TypeRefBuilder.cpp)
target_compile_options(swiftRemoteInspection PRIVATE -DSWIFT_ENABLE_REFLECTION)
target_link_libraries(swiftRemoteInspection PUBLIC
  swiftDemangling)
