
add_swift_host_library(swiftLLVMPasses STATIC
  LLVMSwiftAA.cpp
  LLVMSwiftRCIdentity.cpp
  LLVMARCOpts.cpp
  LLVMARCContract.cpp
  LLVMInlineTree.cpp
  LLVMMergeFunctions.cpp
  LLVMEmitAsyncEntryReturnMetadata.cpp

  LLVM_LINK_COMPONENTS
  analysis
  targetparser
  )
target_link_libraries(swiftLLVMPasses PRIVATE
  swiftDemangling)

set_swift_llvm_is_available(swiftLLVMPasses)
