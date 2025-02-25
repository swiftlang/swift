target_sources(swiftSILOptimizer PRIVATE
  ArrayPropertyOpt.cpp
  BoundsCheckOpts.cpp
  COWArrayOpt.cpp
  LoopRotate.cpp
  LoopUnroll.cpp
  LICM.cpp
  ForEachLoopUnroll.cpp)
