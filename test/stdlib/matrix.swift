func printMatrix(m: Int[][]) {
  println("[")
  for i in 0...m.count {
    print("  [")
    var prefix = ""
    for j in 0...m[i].count {
      print(prefix)
      print(m[i][j])
      prefix = ", "
    }
    println("]")
  }
  println("]")
}

func mkmatrix1(rows: Int, cols: Int) -> Int[][] {
  var count = 1
  var m = new Int[rows][]
  for i in 0 ... rows {
    m[i] = new Int[cols]
    for j in 0 ... cols {
      //FIXME: m[i][j] = count
      var tmp = m[i]
      tmp[j] = count
      ++count
    }
  }
  return m  
}

func mkmatrix(rows: Int, cols: Int) -> Int[][] {
  var count = 1
  var m = Array<Int[]>(rows, Array<Int>(cols, 0))
  for i in 0 ... rows {
    for j in 0 ... cols {
      m[i][j] = count
      ++count
    }
  }
  return m  
}

func mmult(rows: Int, cols: Int, m1: Int[][], m2: Int[][], inout m3: Int[][]) {
  for i in 0 ... rows {
    for j in 0 ... cols {
      var value = 0
      for k in 0 ... cols {
        value = value + m1[i][k] * m2[k][j]
      }
      
      m3[i][j] = value
    }
  }
}

var n = 2
var SIZE = 10

var m1 = mkmatrix(SIZE, SIZE)
var m2 = mkmatrix(SIZE, SIZE)
var mm = mkmatrix(SIZE, SIZE)

printMatrix(m1)

for i in 0 ... n {
  mmult(SIZE, SIZE, m1, m2, &mm)
}

var t1 = mm[0]
var t2 = mm[2]
var t3 = mm[3]
var t4 = mm[4]

// FIXME: Putting this on one line blows up compile time:
// <rdar://problem/14949542> insane compile time on trivial code
var res = String(t1[0])
res += " " + String(t2[3])
res += " " + String(t3[2])
res += " " + String(t4[4])
res += " " + String(n)
res += " " + String(SIZE)
// CHECK: 3355 13320 17865 23575 200 10
println(res)
