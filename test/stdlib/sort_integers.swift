// RUN: %target-run-simple-swift | FileCheck %s

// Generate all possible permutes.
func _permuteInternal(elem: Int, size : Int, perm : Int[], visited : Bool[], verify : (Int[]) -> ()) {
  if (elem == size) {
    verify(perm)
    return
  }

  for i in 0..size {
    if (visited[i]) {
      continue
    }
    visited[i] = true
    perm[elem] = i
    _permuteInternal(elem + 1, size, perm, visited, verify)
    visited[i] = false
  }
}

// Convenience wrapper for the permute method.
func permute(size : Int, verify : (Int[]) -> ()) {
  var perm : Int[] = []
  var visited : Bool[] = []
  for i in 0..size {
    visited.append(false)
    perm.append(0)
  }
  _permuteInternal(0, size, perm, visited, verify)
}


// Verify the permute method itself:
let printer : (Int[]) -> () = {
    print("[")
    for i in $0 {
      print("\(i) ")
    }
    print("]\n")
}
//CHECK: [0 1 2 ]
//CHECK: [0 2 1 ]
//CHECK: [1 0 2 ]
//CHECK: [1 2 0 ]
//CHECK: [2 0 1 ]
//CHECK: [2 1 0 ]
permute(3, printer)

// Now, let's verify the sort.
let verifier : (Int[]) -> () = {
    var y = $0.copy()
    sort(y)
    for i in 0..y.count - 1 {
      if (y[i] > y[i+1]) {
        print("Error!")
        return
      }
    }
}

//CHECK-NOT: Error!
permute(2, verifier)
permute(5, verifier)
permute(6, verifier)
permute(7, verifier)
//CHECK: Done
println("Done")

