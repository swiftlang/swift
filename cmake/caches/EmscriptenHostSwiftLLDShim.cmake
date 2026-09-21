if(NOT SWIFT_EMSCRIPTEN_HOST_LLVM_LIB_DIR)
  message(FATAL_ERROR
    "SWIFT_EMSCRIPTEN_HOST_LLVM_LIB_DIR is not set. This top-level include "
    "expects -DSWIFT_EMSCRIPTEN_HOST_LLVM_LIB_DIR=<emscriptenhostllvm>/lib, "
    "pointing at the EmscriptenHostLLVM build's lib directory (the one "
    "containing liblldWasm.a / liblldCommon.a).")
endif()

foreach(_lldlib lldWasm lldCommon)
  if(NOT TARGET ${_lldlib})
    add_library(${_lldlib} STATIC IMPORTED GLOBAL)
    set_target_properties(${_lldlib} PROPERTIES
      IMPORTED_LOCATION
        "${SWIFT_EMSCRIPTEN_HOST_LLVM_LIB_DIR}/lib${_lldlib}.a")
  endif()
endforeach()
