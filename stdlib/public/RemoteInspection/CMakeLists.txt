set(swiftRemoteInspection_C_COMPILE_FLAGS)
if(SWIFT_ENABLE_REFLECTION)
  list(APPEND swiftRemoteInspection_C_COMPILE_FLAGS -DSWIFT_ENABLE_REFLECTION)
endif()

add_swift_target_library(swiftRemoteInspection STATIC
  MetadataSource.cpp
  TypeLowering.cpp
  TypeRef.cpp
  TypeRefBuilder.cpp
  C_COMPILE_FLAGS
    ${SWIFT_RUNTIME_CXX_FLAGS}
    -DswiftCore_EXPORTS
    ${swiftRemoteInspection_C_COMPILE_FLAGS}
  LINK_FLAGS ${SWIFT_RUNTIME_LINK_FLAGS}
  INCORPORATE_OBJECT_LIBRARIES
    swiftLLVMSupport swiftDemangling swiftDemanglingCR
  SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  INSTALL_IN_COMPONENT dev)
