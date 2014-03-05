add_swift_library(swiftOptimizeARC
  Passes.cpp
  COMPONENT_DEPENDS analysis
  NO_LLVM_COMMON_LIBS
  )

# FIXME this is probably too low-level a dependency
# but it makes LLDB build.
add_dependencies(swiftOptimizeARC intrinsics_gen)
