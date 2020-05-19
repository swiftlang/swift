target_sources(swiftSILOptimizer PRIVATE
  ArrayBoundsCheckOpts.cpp
  ArrayPropertyOpt.cpp
  COWArrayOpt.cpp
  LoopRotate.cpp
  LoopUnroll.cpp
  LICM.cpp
  ForEachLoopUnroll.cpp)
