add_swift_library(swiftSerialization
  DeclTypeRecordNodes.def
  Deserialization.cpp
  DeserializeSIL.cpp
  ModuleFile.cpp
  Serialization.cpp
  SerializedModuleLoader.cpp
  SerializedSILLoader.cpp
  SerializeSIL.cpp
  DEPENDS swiftAST LLVMBitReader LLVMBitWriter LLVMCore)
