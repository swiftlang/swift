// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -constraint-checker -parse -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs -I=%S/Inputs/custom-modules %s -verify

import ObjC

// This import shouldn't be necessary because "Swarm" already imports 
// Foundation, but it only imports the Clang module, which means the Swift
// adapter module isn't present by default. <rdar://problem/12985560>
import Foundation

// Check the use of a typed collection from a different module.
import Swarm

func checkSwarm(s : Swarm, b : B) {
  var b2 : B = s.bees[0]
}
