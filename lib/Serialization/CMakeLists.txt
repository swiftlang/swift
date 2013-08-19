add_swift_library(swiftSerialization
  Deserialization.cpp
  ModuleFile.cpp
  Serialization.cpp
  SerializedModuleLoader.cpp
  DEPENDS swiftAST)
