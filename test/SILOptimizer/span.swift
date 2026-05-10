// RUN: %target-swift-frontend %s -parse-as-library -disable-availability-checking -emit-ir -o /dev/null

// Check that the MoveOnlyWrappedTypeEliminator doesn't crash
func consumingArray(_ arr: consuming [Int]) {
  let s = arr.mutableSpan;
  _ = consume s
}

