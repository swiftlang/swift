if (SWIFT_BUILD_SWIFT_SYNTAX)
  set(plugin_server_dependencies
    SwiftCompilerPluginMessageHandling
    SwiftLibraryPluginProvider)

  if(SWIFT_ENABLE_WASM_PLUGINS)
    # TODO: create new variables instead of piggybacking off of SWIFT_PATH_TO_SWIFT_SYNTAX_SOURCE
    file(TO_CMAKE_PATH "${SWIFT_PATH_TO_SWIFT_SYNTAX_SOURCE}" swift_syntax_path)
    FetchContent_Declare(SwiftSystem SOURCE_DIR "${swift_syntax_path}/../swift-system")
    set(WASMKIT_BUILD_CLI OFF)
    FetchContent_Declare(WasmKit SOURCE_DIR "${swift_syntax_path}/../wasmkit")
    # swift-system sets no compiler target and WasmKit sets only the Swift one, so
    # both would otherwise build for the SDK's newer deployment target.
    set(CMAKE_C_COMPILER_TARGET ${SWIFT_HOST_TRIPLE})
    set(CMAKE_Swift_COMPILER_TARGET ${SWIFT_HOST_TRIPLE})
    FetchContent_MakeAvailable(SwiftSystem WasmKit)
    unset(CMAKE_C_COMPILER_TARGET)
    unset(CMAKE_Swift_COMPILER_TARGET)

    list(APPEND plugin_server_dependencies
      SystemPackage
      WasmKitWASI)
  endif()

  add_pure_swift_host_tool(swift-plugin-server
    Sources/swift-plugin-server/Cleanup.swift
    Sources/swift-plugin-server/SwiftPluginServer.swift
    Sources/swift-plugin-server/WasmEngine.swift
    Sources/swift-plugin-server/WasmInterceptingMessageHandler.swift
    Sources/swift-plugin-server/WasmKitEngine.swift
    Sources/swift-plugin-server/WasmPlugin.swift
    Sources/swift-plugin-server/WasmStandardError.swift
    Sources/swift-plugin-server/WasmStdioBuffer.swift
    Sources/swift-plugin-server/WasmStdioStream.swift
    SWIFT_COMPONENT
      compiler
    SWIFT_DEPENDENCIES
      ${plugin_server_dependencies}
    PACKAGE_NAME Toolchain
  )
  if(SWIFT_ENABLE_WASM_PLUGINS)
    target_compile_definitions(swift-plugin-server PRIVATE
      "$<$<COMPILE_LANGUAGE:Swift>:SWIFT_PLUGIN_SERVER_SUPPORTS_WASM>")
  endif()

  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${SWIFT_HOST_LIBRARIES_DEST_DIR}")
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${SWIFT_HOST_LIBRARIES_DEST_DIR}")
  add_pure_swift_host_library(SwiftInProcPluginServer SHARED
    Sources/SwiftInProcPluginServer/InProcPluginServer.swift
    SWIFT_DEPENDENCIES
      SwiftCompilerPluginMessageHandling
      SwiftLibraryPluginProvider
    PACKAGE_NAME Toolchain
  )

  if(SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_DARWIN_PLATFORMS)
    set_property(TARGET SwiftInProcPluginServer
      APPEND PROPERTY INSTALL_RPATH
      "@loader_path")
  elseif(SWIFT_HOST_VARIANT_SDK MATCHES "LINUX|ANDROID|OPENBSD|FREEBSD")
    set_property(TARGET SwiftInProcPluginServer
      APPEND PROPERTY INSTALL_RPATH
        "$ORIGIN")
    _set_pure_swift_link_flags(SwiftInProcPluginServer "../../")
  endif()

  add_dependencies(compiler SwiftInProcPluginServer)
  swift_install_in_component(TARGETS SwiftInProcPluginServer
    ARCHIVE DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/host" COMPONENT compiler
    LIBRARY DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/host" COMPONENT compiler
    RUNTIME DESTINATION "bin" COMPONENT compiler)
endif()
