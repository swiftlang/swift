if (SWIFT_BUILD_SWIFT_SYNTAX)
  # override the remote SwiftSystem and ArgParser dependencies in WasmKit
  FetchContent_Declare(SwiftSystem SOURCE_DIR "${PROJECT_SOURCE_DIR}/../swift-system")
  FetchContent_Declare(ArgumentParser SOURCE_DIR "${PROJECT_SOURCE_DIR}/../swift-argument-parser")

  FetchContent_Declare(WasmKit SOURCE_DIR "${PROJECT_SOURCE_DIR}/../wasmkit")
  FetchContent_MakeAvailable(WasmKit)

  # _swiftCSwiftPluginServer is just a C support library for swift-plugin-server
  # Don't bother to create '.a' for that.
  add_swift_host_library(_swiftCSwiftPluginServer STATIC
    Sources/CSwiftPluginServer/PluginServer.cpp
    LLVM_LINK_COMPONENTS support
  )
  target_link_libraries(_swiftCSwiftPluginServer PRIVATE
    swiftDemangling
  )
  target_include_directories(_swiftCSwiftPluginServer PUBLIC
    Sources/CSwiftPluginServer/include
  )

  add_pure_swift_host_tool(swift-plugin-server
    Sources/swift-plugin-server/swift-plugin-server.swift
    DEPENDENCIES
      _swiftCSwiftPluginServer
    SWIFT_COMPONENT
      compiler
    SWIFT_DEPENDENCIES
      SwiftSyntaxMacros
      SwiftSyntaxMacroExpansion
      SwiftCompilerPluginMessageHandling
  )
  target_include_directories(swift-plugin-server PRIVATE
    Sources/CSwiftPluginServer/include
  )

  set(SWIFT_WASM_ALLOW_JSC ON CACHE BOOL "Use JavaScriptCore Wasm runtime if possible" FORCE)

  if(SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_DARWIN_PLATFORMS)
    set(swift_wasm_use_jsc ${SWIFT_WASM_ALLOW_JSC})
  else()
    set(swift_wasm_use_jsc OFF)
  endif()

  if (swift_wasm_use_jsc)
    set(wasi_dep WASI)
    set(jsc_defines SWIFT_WASM_USE_JSC)
  else()
    set(wasi_dep WasmKitWASI)
    set(jsc_defines)
  endif()

  add_pure_swift_host_tool(swift-wasm-plugin-server
    Sources/swift-wasm-plugin-server/swift-wasm-plugin-server.swift
    Sources/swift-wasm-plugin-server/JSCWasmEngine.swift
    Sources/swift-wasm-plugin-server/WasmEngine.swift
    Sources/swift-wasm-plugin-server/WasmKitEngine.swift
    SWIFT_COMPONENT
      compiler
    SWIFT_DEPENDENCIES
      SwiftCompilerPluginMessageHandling
      ${wasi_dep}
  )
  target_compile_definitions(swift-wasm-plugin-server PRIVATE ${jsc_defines})
  target_include_directories(swift-wasm-plugin-server PRIVATE
    Sources/CSwiftPluginServer/include
  )
  if (swift_wasm_use_jsc)
    add_custom_command(TARGET swift-wasm-plugin-server POST_BUILD COMMAND
      codesign -fs - $<TARGET_FILE:swift-wasm-plugin-server> --entitlements ${CMAKE_CURRENT_SOURCE_DIR}/allow-jit.plist
    )
  endif()
endif()
