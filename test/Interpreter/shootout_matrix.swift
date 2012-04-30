// RUN: %swift -I %S/.. %s -i | FileCheck %s

// CHECK: 3355 13320 17865 23575

func mkmatrix(rows : Int, cols : Int) -> Int[][] {
  var count = 1
  var m = new Int[rows][]
  foreach i in 0 .. rows {
    m[i] = new Int[cols]
    foreach j in 0 .. cols {
      //FIXME: m[i][j] = count
      var tmp = m[i]
      tmp[j] = count
      ;++count
    }
  }
  return m  
}

func mmult(rows : Int, cols : Int, m1 : Int[][], m2 : Int[][], m3 : Int[][]) {
  foreach i in 0 .. rows {
    foreach j in 0 .. cols {
      var val = 0
      foreach k in 0 .. cols {
        // FIXME: val = val + m1[i][k] * m2[k][j]
        var t1 = m1[i]
        var t2 = m2[k]
        val = val + t1[k] * t2[j]
      }
      
      // FIXME: m3[i][j] = val
      var t3 = m3[i]
      t3[j] = val
    }
  }
}

var n = 30
var SIZE = 10

var m1 = mkmatrix(SIZE, SIZE)
var m2 = mkmatrix(SIZE, SIZE)
var mm = mkmatrix(SIZE, SIZE)

foreach i in 0 .. n {
  mmult(SIZE, SIZE, m1, m2, mm)
}

// FIXME: String interpolation.
//println("%(mm[0][0]) %(mm[2][3]) %(mm[3][2]) %(mm[4][4]))

// FIXME: Multisubscript.
//println(String(mm[0][0]) + " " + String(mm[2][3]) + " " +
//        String(mm[3][2]) + " " + String(mm[4][4]))
  
var t1 = mm[0]
var t2 = mm[2]
var t3 = mm[3]
var t4 = mm[4]
println(String(t1[0]) + " " + String(t2[3]) + " " +
        String(t3[2]) + " " + String(t4[4]))      
