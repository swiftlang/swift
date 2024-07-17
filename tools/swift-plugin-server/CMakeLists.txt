if (SWIFT_BUILD_SWIFT_SYNTAX)
  add_pure_swift_host_tool(swift-plugin-server
    Sources/swift-plugin-server/swift-plugin-server.swift
    SWIFT_COMPONENT
      compiler
    SWIFT_DEPENDENCIES
      SwiftCompilerPluginMessageHandling
      SwiftLibraryPluginProvider
  )

  # FXIME: Importing package-cmo enabled '.swiftmodule' causes compiler crash :(
  if(Swift_COMPILER_PACKAGE_CMO_SUPPORT)
    target_compile_options(swift-plugin-server PRIVATE
      "SHELL:-Xfrontend -module-load-mode -Xfrontend prefer-interface")
  endif()
endif()
