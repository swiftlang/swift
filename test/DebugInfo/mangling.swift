// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s
// mangling.myDict : swift.Dictionary<swift.Int64, swift.String>
// CHECK: _T8mangling6myDictGCSs10DictionarySiSS_
var myDict = Dictionary<Int, String>()
myDict.add(12, "Hello!")

// mangling.myTuple1 : (Name : swift.String, Id : swift.Int64)
// CHECK: _T8mangling8myTuple1T4NameSS2IdSi_
var myTuple1 : (Name: String, Id: Int) = ("A", 1)
// mangling.myTuple2 : (swift.String, Id : swift.Int64)
// CHECK: _T8mangling8myTuple2TSS2IdSi_
var myTuple2 : (      String, Id: Int) = ("B", 2)
// mangling.myTuple3 : (swift.String, swift.Int64)
// CHECK: _T8mangling8myTuple3TSSSi_
var myTuple3 : (      String,     Int) = ("C", 3)

println(myTuple1.Id)
println(myTuple2.Id)
println({ $1 }(myTuple3))

// mangling.ExistentialTuple <A : swift.RandomAccessIndex, B>(x : A, y : A) -> B
// CHECK: _T8mangling16ExistentialTupleUSs17RandomAccessIndex___FT1xQ_1yQ__Q0_
func ExistentialTuple<T: RandomAccessIndex>(x: T, y: T) -> T.DistanceType {
  // (B, swift.Bool)
  // CHECK: _TtTQ0_Sb_
  var tmp : (T.DistanceType, Bool) = T.sub(x, y)
  alwaysTrap(tmp.1 == false)
  return tmp.0
}

