// RUN: %target-swift-emit-sil -O  -Xllvm -sil-inline-generics=false -Xllvm -sil-partial-specialization=false -primary-file %s -o - | %FileCheck %s

// check if the compiler does not crash if a function is specialized
// which contains a collection cast

class MyClass {}

class KeyClass : Hashable {
  func hash(into hasher: inout Hasher) {}
}
func ==(lhs: KeyClass, rhs: KeyClass) -> Bool { return true }

// CHECK-LABEL: sil{{.*}}@{{.*}}arrayUpCast{{.*}} <Ct where Ct : MyClass>
func arrayUpCast<Ct: MyClass>(_ arr: [Ct]) -> [MyClass] {
  // CHECK: apply %{{[0-9]*}}<Ct{{.*}}>(%{{[0-9]*}})
  return arr
  // CHECK: return	  
}

// CHECK-LABEL: sil{{.*}}@{{.*}}arrayDownCast{{.*}} <Ct where Ct : MyClass>
func arrayDownCast<Ct: MyClass>(_ arr: [MyClass]) -> [Ct] {
  // CHECK: apply %{{[0-9]*}}<{{.*}}Ct>(%{{[0-9]*}})
  return arr as! [Ct]
  // CHECK: return	  
}

// CHECK-LABEL: sil{{.*}}@{{.*}}dictUpCast{{.*}} <Ct where Ct : MyClass>
func dictUpCast<Ct: MyClass>(_ dict: [KeyClass:Ct]) -> [KeyClass:MyClass] {
  // CHECK: apply %{{[0-9]*}}<{{.*}}Ct{{.*}}>(%{{[0-9]*}})
  return dict as [KeyClass:MyClass]
  // CHECK: return	  
}

// CHECK-LABEL: sil{{.*}}@{{.*}}dictDownCast{{.*}} <Ct where Ct : MyClass>
func dictDownCast<Ct: MyClass>(_ dict: [KeyClass:MyClass]) -> [KeyClass:Ct] {
  // CHECK: apply %{{[0-9]*}}<KeyClass, MyClass, KeyClass, Ct>(%{{[0-9]*}})
  return dict as! [KeyClass:Ct]
  // CHECK: return	  
}

func setUpCast<Ct: KeyClass>(_ s: Set<Ct>) -> Set<KeyClass> {
  // CHECK: apply %{{[0-9]*}}<Ct, KeyClass>(%{{[0-9]*}})
  return s as Set<KeyClass>
  // CHECK: return	  
}

func setDownCast<Ct : KeyClass>(_ s : Set<KeyClass>) -> Set<Ct> {
  // CHECK: apply %{{[0-9]*}}<KeyClass, Ct>(%{{[0-9]*}})
  return s as! Set<Ct>
  // CHECK: return	  
}

let arr: [MyClass] = [MyClass()]

arrayUpCast(arr)
arrayDownCast(arr)

let dict: [KeyClass:MyClass] = [KeyClass() : MyClass()]

dictUpCast(dict)
dictDownCast(dict)

let s: Set<KeyClass> = Set()

setUpCast(s)
setDownCast(s)
