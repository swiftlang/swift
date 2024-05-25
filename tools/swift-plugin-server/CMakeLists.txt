if (SWIFT_BUILD_SWIFT_SYNTAX)
  # override the remote SwiftSystem and ArgParser dependencies in WasmKit
  FetchContent_Declare(SwiftSystem SOURCE_DIR "${PROJECT_SOURCE_DIR}/../swift-system")
  FetchContent_Declare(ArgumentParser SOURCE_DIR "${PROJECT_SOURCE_DIR}/../swift-argument-parser")

  FetchContent_Declare(WasmKit SOURCE_DIR "${PROJECT_SOURCE_DIR}/../wasmkit")
  FetchContent_MakeAvailable(WasmKit)

  add_pure_swift_host_tool(swift-plugin-server
    Sources/swift-plugin-server/swift-plugin-server.swift
    Sources/swift-plugin-server/WasmEngine.swift
    Sources/swift-plugin-server/WasmKitEngine.swift
    Sources/swift-plugin-server/WasmMessageHandler.swift
    SWIFT_COMPONENT
      compiler
    SWIFT_DEPENDENCIES
      SwiftCompilerPluginMessageHandling
      SwiftLibraryPluginProvider
      SystemPackage
      ${wasi_dep}
  )
endif()
