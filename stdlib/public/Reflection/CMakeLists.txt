add_swift_library(swiftReflection TARGET_LIBRARY HOST_LIBRARY
  Demangle.cpp
  MetadataSource.cpp
  Remangle.cpp
  TypeLowering.cpp
  TypeRef.cpp
  TypeRefBuilder.cpp
  INSTALL_IN_COMPONENT dev)
