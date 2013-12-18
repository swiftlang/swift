// RUN: %swift -i %s | FileCheck %s
// REQUIRES: swift_interpreter

class HeapBuf<Element> {}

struct XBuffer {
 var _storage: HeapBuf<()>
}

class Q {}

func hexAddr(x: DynamicLookup) -> Q {
 return Q()
}

func crash() {
 hexAddr(XBuffer(HeapBuf<()>())._storage)
}

crash()

// CHECK: ok
println("ok")
