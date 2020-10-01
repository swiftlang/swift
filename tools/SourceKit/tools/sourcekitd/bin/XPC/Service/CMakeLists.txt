if (NOT SOURCEKIT_INSTALLING_INPROC)
  add_sourcekit_xpc_service(SourceKitService sourcekitd
    XPCService.cpp
    LLVM_LINK_COMPONENTS support coverage
  )
  target_link_libraries(SourceKitService PRIVATE
    SourceKitSwiftLang
    sourcekitdAPI)
endif()

if (NOT SOURCEKIT_DEPLOYMENT_OS MATCHES "^macosx")
  add_definitions(-DSOURCEKIT_UNVERSIONED_FRAMEWORK_BUNDLE)
endif()
