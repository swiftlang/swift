# Runtime library for Swift ARC and memory management

add_library(swiftcRuntime STATIC
  ARC.cpp
)

target_include_directories(swiftcRuntime PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

# Link with threading support for atomic operations
find_package(Threads REQUIRED)
target_link_libraries(swiftcRuntime Threads::Threads)