add_swift_library(swiftReflection STATIC TARGET_LIBRARY FORCE_BUILD_FOR_HOST_SDK
  Demangle.cpp
  MetadataSource.cpp
  Remangle.cpp
  TypeLowering.cpp
  TypeRef.cpp
  TypeRefBuilder.cpp
  INSTALL_IN_COMPONENT dev)
