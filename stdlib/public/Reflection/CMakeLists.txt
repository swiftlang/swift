set(swiftReflection_SOURCES
  MetadataSource.cpp
  TypeLowering.cpp
  TypeRef.cpp
  TypeRefBuilder.cpp)

add_swift_target_library(swiftReflection STATIC
  ${swiftReflection_SOURCES}
  C_COMPILE_FLAGS ${SWIFT_RUNTIME_CXX_FLAGS} -DswiftCore_EXPORTS
  LINK_FLAGS ${SWIFT_RUNTIME_LINK_FLAGS}
  INCORPORATE_OBJECT_LIBRARIES
    swiftLLVMSupport swiftDemangling
  SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  INSTALL_IN_COMPONENT dev)
