func something(obj: Int) -> Int { return obj }
func something(a: Int, b: Int) -> (Int, Int) { return (a, b) }
func something(a: Int, b: Int, c: Int) -> (Int, Int, Int) { return (a, b, c) }

func ambiguousWithVar(_: Int) {}
func scopedVar(_: Int) {}
func localVar(_: Int) {}
func scopedFunction(value: Int) -> Int { return value }

func TypeNameWins(value: Int) -> Int { return value }
