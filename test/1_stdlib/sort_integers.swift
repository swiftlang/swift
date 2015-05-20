// RUN: %target-run-simple-swift | FileCheck %s

// Generate all possible permutes.
func _permuteInternal(
  elem: Int, _ size : Int,
  inout _ perm : [Int], inout _ visited : [Bool],
  _ verify : ([Int]) -> ()
) {
  if (elem == size) {
    verify(perm)
    return
  }

  for i in 0..<size {
    if (visited[i]) {
      continue
    }
    visited[i] = true
    perm[elem] = i
    _permuteInternal(elem + 1, size, &perm, &visited, verify)
    visited[i] = false
  }
}

// Convenience wrapper for the permute method.
func permute(size : Int, _ verify : ([Int]) -> ()) {
  var perm = [Int](count: size, repeatedValue: 0)
  var visited = [Bool](count: size, repeatedValue: false)
  _permuteInternal(0, size, &perm, &visited, verify)
}


// A simple random number generator.
func randomize(size : Int, _ verify : ([Int]) -> ()) {
  var arr : [Int] = []
  var N = 1
  var M = 1
  for i in 0..<size {
    N = N * 19 % 1024
    M = (N + M) % size
    arr.append(N)
    if (M % 3 == 0) {
      arr.append(M)
    }
  }
  verify(arr)
}

// Verify the permute method itself:
let printer : ([Int]) -> () = {
  print($0)
}
//CHECK: [0, 1, 2]
//CHECK: [0, 2, 1]
//CHECK: [1, 0, 2]
//CHECK: [1, 2, 0]
//CHECK: [2, 0, 1]
//CHECK: [2, 1, 0]
permute(3, printer)

// Now, let's verify the sort.
let sort_verifier : ([Int]) -> () = {
    var y = $0.sort()
    for i in 0..<y.count - 1 {
    if (y[i] > y[i+1]) {
        print("Error: \(y)")
        return
      }
    }
}

//CHECK-NOT: Error!
permute(2, sort_verifier)
permute(6, sort_verifier)
permute(7, sort_verifier)
//CHECK: Test1 - Done
print("Test1 - Done")

// Now, let's verify the sort.
let partition_verifier : ([Int]) -> () = {
    var y = $0
    // Partition() returns the index to the pivot value.
    let idx = y.partition(0..<y.count)
    // Check that all of the elements in the first partition are smaller or
    // equal to the pivot value.
    for i in 0..<idx {
      if y[i] > y[idx]  {
        print("Error!\n", appendNewline: false)
        return
      }
    }
    // Check that all of the elements in the second partition are greater or
    // equal to the pivot value.
    for i in idx..<y.count - 1 {
      if y[i] < y[idx]  {
        print("Error!\n", appendNewline: false)
        return
      }
    }
}

// FIXME(prext): remove partition() together with the function when
// protocol extensions land.  These tests have been migrated to the new API.
permute(2, partition_verifier)
permute(6, partition_verifier)
permute(7, partition_verifier)
//CHECK-NOT: Error!
//CHECK: Test2 - Done
print("Test2 - Done")


randomize(70, sort_verifier)
randomize(700, sort_verifier)
randomize(1900, sort_verifier)
//CHECK-NOT: Error!
//CHECK: Test3 - Done
print("Test3 - Done")

