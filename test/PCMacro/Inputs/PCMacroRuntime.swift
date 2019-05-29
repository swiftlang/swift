// This is the minimal amount of runtime required to operate 
// lib/Sema/PCMacro.cpp successfully.

public func __builtin_pc_before(_ sl : Int, _ el : Int, _ sc : Int, _ ec: Int, _ moduleId : Int, _ fileId : Int) {
  print("[\(moduleId):\(fileId)] [\(sl):\(sc)-\(el):\(ec)] pc before")
}

public func __builtin_pc_after(_ sl : Int, _ el : Int, _ sc : Int, _ ec: Int, _ moduleId : Int, _ fileId : Int) {
  print("[\(moduleId):\(fileId)] [\(sl):\(sc)-\(el):\(ec)] pc after")
}
