# Emit symbol graphs as part of the normal Swift compile.
#
# Enabled by `${PROJECT_NAME}_ENABLE_SYMBOL_GRAPHS`. This writes Symbol graphs to
# `${CMAKE_BINARY_DIR}/symbol-graphs`.
function(emit_symbol_graph target)
  if(NOT ${PROJECT_NAME}_ENABLE_SYMBOL_GRAPHS)
    return()
  endif()

  set(symbol_graph_dir "${CMAKE_BINARY_DIR}/symbol-graphs")
  file(MAKE_DIRECTORY "${symbol_graph_dir}")

  target_compile_options(${target} PRIVATE
    "$<$<COMPILE_LANGUAGE:Swift>:-emit-symbol-graph>"
    "$<$<COMPILE_LANGUAGE:Swift>:-emit-symbol-graph-dir$<SEMICOLON>${symbol_graph_dir}>"
    "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -symbol-graph-allow-availability-platforms -Xfrontend Swift>")
  # Extensions of other modules are emitted as `<module>@<extended>.symbols.json`.
  # `ADDITIONAL_CLEAN_FILES` does not expand globs, so clean the whole directory.
  set_property(TARGET ${target} APPEND PROPERTY ADDITIONAL_CLEAN_FILES
    "${symbol_graph_dir}")
endfunction()
