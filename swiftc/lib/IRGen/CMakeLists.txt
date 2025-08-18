set(SOURCES
  IRGen.cpp
)

add_library(swiftcIRGen STATIC ${SOURCES})

target_include_directories(swiftcIRGen PUBLIC
  ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

target_link_libraries(swiftcIRGen swiftcBasic swiftcSIL ${llvm_libs})