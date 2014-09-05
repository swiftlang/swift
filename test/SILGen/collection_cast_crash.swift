// RUN: %swift -O -target x86_64-apple-macosx10.9 %s -emit-sil -o - | FileCheck %s

// check if the compiler does not crash if a function is specialized
// which contains a collection cast

class MyClass {}

class KeyClass : Hashable {
	var hashValue : Int { return 0 }
}
func ==(lhs: KeyClass, rhs: KeyClass) -> Bool { return true }

// CHECK-LABEL: sil @{{.*}}arrayUpCast{{.*}} <Ct where Ct : MyClass>
func arrayUpCast<Ct: MyClass>(arr: [Ct]) -> [MyClass] {
  // CHECK: apply %{{[0-9]*}}<Ct, MyClass>(%{{[0-9]*}})
  return arr
  // CHECK: return	  
}

// CHECK-LABEL: sil @{{.*}}arrayDownCast{{.*}} <Ct where Ct : MyClass>
func arrayDownCast<Ct: MyClass>(arr: [MyClass]) -> [Ct] {
  // CHECK: apply %{{[0-9]*}}<MyClass, Ct>(%{{[0-9]*}})
  return arr as [Ct]
  // CHECK: return	  
}

// CHECK-LABEL: sil @{{.*}}dictUpCast{{.*}} <Ct where Ct : MyClass>
func dictUpCast<Ct: MyClass>(dict: [KeyClass:Ct]) -> [KeyClass:MyClass] {
  // CHECK: apply %{{[0-9]*}}<KeyClass, Ct, KeyClass, MyClass>(%{{[0-9]*}})
  return dict as [KeyClass:MyClass]
  // CHECK: return	  
}

// CHECK-LABEL: sil @{{.*}}dictDownCast{{.*}} <Ct where Ct : MyClass>
func dictDownCast<Ct: MyClass>(dict: [KeyClass:MyClass]) -> [KeyClass:Ct] {
  // CHECK: apply %{{[0-9]*}}<KeyClass, MyClass, KeyClass, Ct>(%{{[0-9]*}})
  return dict as [KeyClass:Ct]
  // CHECK: return	  
}


let arr: [MyClass] = [MyClass()]

arrayUpCast(arr)
arrayDownCast(arr)

let dict: [KeyClass:MyClass] = [KeyClass() : MyClass()]

dictUpCast(dict)
dictDownCast(dict)
