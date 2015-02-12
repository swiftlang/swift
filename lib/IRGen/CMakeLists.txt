add_swift_library(swiftIRGen
  DebugTypeInfo.cpp
  ExtraInhabitants.cpp
  GenCast.cpp
  GenClangDecl.cpp
  GenClangType.cpp
  GenClass.cpp
  GenControl.cpp
  GenCoverage.cpp
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
  TypeLayoutVerifier.cpp
  UnimplementedTypeInfo.cpp
  LINK_LIBRARIES
    swiftAST
    swiftLLVMPasses

    # Clang dependencies.
    # FIXME: Clang should really export these in some reasonable manner.
    clangCodeGen
  COMPONENT_DEPENDS profiledata instrumentation)
