// RUN: %target-run-simple-swift | FileCheck %s

class HeapBuf<Element> {}

struct XBuffer {
 var _storage: HeapBuf<()>
}

class Q {}

func hexAddr(x: AnyObject) -> Q {
 return Q()
}

func crash() {
 hexAddr(XBuffer(_storage: HeapBuf<()>())._storage)
}

crash()

// CHECK: ok
print("ok")
