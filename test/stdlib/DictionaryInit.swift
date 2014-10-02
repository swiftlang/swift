// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out -Ounchecked
//
// RUN: %target-run %t/a.out 2>&1 | FileCheck %s

@inline(never)
public func run_Dictionary() -> Dictionary<Int, Bool> {

  // CSE should not be able to combine both Dictionary.init() calls.
  // This did happen and resulted in a crash because Dictionary.init()
  // was defined with @effects(readnone).
  // But this was wrong because it actually reads the array buffer (from
  // the literal).
  var Dict: Dictionary<Int, Bool> = [:]
  Dict = [:]
  Dict[0] = true;
  return Dict
}

let dict = run_Dictionary()
println("result=\(dict[0])")
// CHECK: result=Optional(true)

