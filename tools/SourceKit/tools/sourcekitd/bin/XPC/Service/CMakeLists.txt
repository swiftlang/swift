if (NOT SOURCEKIT_INSTALLING_INPROC)
  add_sourcekit_xpc_service(SourceKitService sourcekitd
    XPCService.cpp
    LINK_LIBS SourceKitSwiftLang sourcekitdAPI
    LLVM_COMPONENT_DEPENDS support coverage
  )
endif()

if (NOT SOURCEKIT_DEPLOYMENT_OS MATCHES "^macosx")
  add_definitions(-DSOURCEKIT_UNVERSIONED_FRAMEWORK_BUNDLE)
endif()
