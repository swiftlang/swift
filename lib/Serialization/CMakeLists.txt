add_swift_library(swiftSerialization
  ModuleFile.cpp
  Serialization.cpp
  SerializedModuleLoader.cpp
  DEPENDS swiftAST)
