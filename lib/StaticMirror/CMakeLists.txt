add_swift_host_library(swiftStaticMirror STATIC
   BinaryScanningTool.cpp
   ObjectFileContext.cpp
   LLVM_LINK_COMPONENTS object support)

 target_link_libraries(swiftStaticMirror PRIVATE
     swiftRemoteInspection)
