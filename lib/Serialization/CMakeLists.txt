add_swift_library(swiftSerialization STATIC
  Deserialization.cpp
  DeserializeSIL.cpp
  ModuleFile.cpp
  Serialization.cpp
  SerializedModuleLoader.cpp
  SerializedSILLoader.cpp
  SerializeSIL.cpp
  LINK_LIBS
    swiftClangImporter)

