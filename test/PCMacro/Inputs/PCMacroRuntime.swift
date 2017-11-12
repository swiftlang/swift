// This is the minimal amount of runtime required to operate 
// lib/Sema/PCMacro.cpp successfully.

func __builtin_pc_before(_ sl : Int, _ el : Int, _ sc : Int, _ ec: Int) {
  print("[\(sl):\(sc)-\(el):\(ec)] pc before")
}

func __builtin_pc_after(_ sl : Int, _ el : Int, _ sc : Int, _ ec: Int) {
  print("[\(sl):\(sc)-\(el):\(ec)] pc after")
}
