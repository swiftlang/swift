# libswiftRemoteMirror.dylib should not have runtime dependencies; it's
# always built as a shared library.
if(SWIFT_BUILD_DYNAMIC_STDLIB)
  add_swift_target_library(swiftRemoteMirror
                           SHARED DONT_EMBED_BITCODE NOSWIFTRT
                           SwiftRemoteMirror.cpp
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/APFloat.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/ARMTargetParser.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/APInt.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/CommandLine.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/ConvertUTF.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/ConvertUTFWrapper.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/CrashRecoveryContext.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/Debug.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/Errno.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/Error.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/ErrorHandling.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/FoldingSet.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/FormatVariadic.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/Hashing.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/Host.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/MD5.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/ManagedStatic.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/MemAlloc.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/MemoryBuffer.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/NativeFormatting.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/Path.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/Program.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/Process.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/Signals.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/SmallPtrSet.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/SmallVector.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/SourceMgr.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/StringExtras.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/StringMap.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/StringRef.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/StringSaver.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/ThreadLocal.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/Threading.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/Triple.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/Twine.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/Unicode.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/WithColor.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/VirtualFileSystem.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/YAMLParser.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/raw_ostream.cpp"
                           LINK_LIBRARIES
                             swiftReflection
                           C_COMPILE_FLAGS
                             ${SWIFT_RUNTIME_CXX_FLAGS} -DswiftRemoteMirror_EXPORTS
                           LINK_FLAGS
                             ${SWIFT_RUNTIME_LINK_FLAGS}
                           SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
                           INSTALL_IN_COMPONENT
                             swift-remote-mirror)
endif()
