set(SOURCES
  SourceManager.cpp
  Diagnostic.cpp
)

add_library(swiftcBasic STATIC ${SOURCES})

target_include_directories(swiftcBasic PUBLIC
  ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

target_link_libraries(swiftcBasic ${llvm_libs})