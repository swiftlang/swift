function(swift_wasm_include_for_arch arch var)
  set(paths)
  list(APPEND paths
       "${SWIFT_WASM_EMSCRIPTEN_PATH}/system/include/libcxx"
       "${SWIFT_WASM_EMSCRIPTEN_PATH}/system/lib/libcxxabi/include"
       "${SWIFT_WASM_EMSCRIPTEN_PATH}/system/include/compat"
       "${SWIFT_WASM_EMSCRIPTEN_PATH}/system/include"
       "${SWIFT_WASM_EMSCRIPTEN_PATH}/system/include/libc"
       "${SWIFT_WASM_EMSCRIPTEN_PATH}/system/lib/libc/musl/arch/emscripten"
       "${SWIFT_WASM_EMSCRIPTEN_PATH}/system/local/include"
       "${SWIFT_WASM_EMSCRIPTEN_PATH}/system/include/SDL") 
  set(${var} ${paths} PARENT_SCOPE)
endfunction()

function(swift_wasm_lib_for_arch arch var)
endfunction()
