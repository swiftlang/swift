add_swift_library(swiftIRGen
  DebugTypeInfo.cpp
  GenClangType.cpp
  GenClass.cpp
  GenControl.cpp
  GenDecl.cpp
  GenFunc.cpp
  GenHeap.cpp
  GenInit.cpp
  GenMeta.cpp
  GenObjC.cpp
  GenEnum.cpp
  GenOpaque.cpp
  GenPoly.cpp
  GenProto.cpp
  GenStruct.cpp
  GenTuple.cpp
  GenType.cpp
  IRGen.cpp
  IRGenDebugInfo.cpp
  IRGenFunction.cpp
  IRGenModule.cpp
  IRGenSIL.cpp
  Linking.cpp
  SwiftTargetInfo.cpp
  StructLayout.cpp
  DEPENDS LLVMMC LLVMProfileData LLVMSupport LLVMCore swiftAST swiftOptimizeARC)

# Clang dependencies.
# FIXME: Clang should really export these in some reasonable manner.
target_link_libraries(swiftIRGen
  clangCodeGen)

