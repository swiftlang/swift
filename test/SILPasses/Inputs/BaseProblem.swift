public class BaseProblem
{
    func run() -> Int
    {
        println("Base class implementation")
        return 0
    }
}

class Evaluator
{
    var map: [Int: () -> Int] = [:]

    init()
    {
        map[1] = { Problem1().run() }
        map[2] = { Problem2().run() }
    }

    func evaluate(n: Int)
    {
        if let problemBlock = map[n]
        {
            let foo = problemBlock()
            println("foo = \(foo)")
        }
    }
}
