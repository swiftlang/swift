func something(obj : Int) -> Int { return obj }
func something(a : Int, b : Int) -> (Int, Int) { return (a, b) }
func something(a : Int, b : Int, c : Int) -> (Int, Int, Int) { return (a, b, c) }

func ambiguousWithVar(val : Int) {}
func scopedVar(val : Int) {}
func localVar(val : Int) {}
func scopedFunction(val : Int) -> Int { return val }

func TypeNameWins(val : Int) -> Int { return val }
