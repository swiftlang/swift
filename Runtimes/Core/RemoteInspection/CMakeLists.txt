
add_library(swiftRemoteInspection OBJECT
  MetadataSource.cpp
  TypeLowering.cpp
  TypeRef.cpp
  TypeRefBuilder.cpp)
target_compile_definitions(swiftRemoteInspection PRIVATE
  $<$<BOOL:${SwiftCore_ENABLE_REFLECTION}>:SWIFT_ENABLE_REFLECTION>)
target_include_directories(swiftRemoteInspection PRIVATE
  "${PROJECT_BINARY_DIR}/include"
  "${SwiftCore_SWIFTC_SOURCE_DIR}/include")
target_include_directories(swiftRemoteInspection PUBLIC
  "$<BUILD_INTERFACE:${SwiftCore_SWIFTC_SOURCE_DIR}/include/swift/RemoteInspection/RuntimeHeaders>")
target_link_libraries(swiftRemoteInspection PRIVATE
  swiftDemangling
  $<$<BOOL:${SwiftCore_ENABLE_CRASH_REPORTER_CLIENT}>:swiftDemanglingCR>
  swiftLLVMSupport
  swiftShims)

if(NOT BUILD_SHARED_LIBS)
  install(TARGETS swiftRemoteInspection
    EXPORT SwiftCoreTargets
    COMPONENT SwiftCore_runtime)
endif()
