set(swiftReflection_SOURCES
  MetadataSource.cpp
  TypeLowering.cpp
  TypeRef.cpp
  TypeRefBuilder.cpp)

set(swiftReflection_C_COMPILE_FLAGS -DswiftCore_EXPORTS)
if(SWIFT_ENABLE_REFLECTION)
  list(APPEND swiftReflection_C_COMPILE_FLAGS -DSWIFT_ENABLE_REFLECTION)
endif()

add_swift_target_library(swiftReflection STATIC
  ${swiftReflection_SOURCES}
  C_COMPILE_FLAGS ${SWIFT_RUNTIME_CXX_FLAGS} ${swiftReflection_C_COMPILE_FLAGS}
  LINK_FLAGS ${SWIFT_RUNTIME_LINK_FLAGS}
  INCORPORATE_OBJECT_LIBRARIES
    swiftLLVMSupport swiftDemangling swiftDemanglingCR
  SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  INSTALL_IN_COMPONENT dev)
