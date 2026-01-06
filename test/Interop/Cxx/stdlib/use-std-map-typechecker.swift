// RUN: not %target-swift-frontend %s -typecheck -I %S/Inputs -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s

import StdMap
import CxxStdlib

func takeCopyable<T: Copyable>(_ x: T) {} 

let mapNonCopyableKey = MapNonCopyableKey() 
takeCopyable(mapNonCopyableKey) 
// CHECK: error: global function 'takeCopyable' requires that 'MapNonCopyableKey' {{.*}} conform to 'Copyable'
// CHECK: note: 'where T: Copyable' is implicit here

let mapNonCopyableValue = MapNonCopyableValue() 
takeCopyable(mapNonCopyableValue) 
// CHECK: error: global function 'takeCopyable' requires that 'MapNonCopyableValue' {{.*}} conform to 'Copyable'
// CHECK: note: 'where T: Copyable' is implicit here
