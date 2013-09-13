add_swift_library(swiftSerialization
  Deserialization.cpp
  DeserializeSIL.cpp
  ModuleFile.cpp
  Serialization.cpp
  SerializedModuleLoader.cpp
  SerializedSILLoader.cpp
  SerializeSIL.cpp
  DEPENDS swiftAST)
