
add_swift_host_library(swiftSerialization STATIC
  Deserialization.cpp
  DeserializeSIL.cpp
  ModuleFile.cpp
  ModuleFileSharedCore.cpp
  ScanningLoaders.cpp
  SnapshotDeserializer.cpp
  SnapshotSerializer.cpp
  Serialization.cpp
  ASTCacheSerializer.cpp
  BodyASTSerializer.cpp
  BodyASTDeserializer.cpp
  SerializedModuleLoader.cpp
  SerializedSILLoader.cpp
  SerializeDoc.cpp
  SerializeSIL.cpp

  LLVM_LINK_COMPONENTS
    BitstreamReader
  )
target_link_libraries(swiftSerialization PRIVATE
  swiftClangImporter
  swiftOption
  swiftAST
  swiftSIL
  clangIndex)

set_swift_llvm_is_available(swiftSerialization)
