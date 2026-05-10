// RUN: %target-swift-frontend %s -emit-sil -sil-verify-all > /dev/null
// RUN: %target-swift-frontend %s -emit-sil -sil-verify-all -O > /dev/null

// https://github.com/apple/swift/issues/61041

public struct S<T> 
{
}
extension S<Int?> 
{
    public mutating 
    func foo(x:inout [Int: Int])
    {
        for _:Int in 0 ... 1
        {
            if let y:Int? = 0 as Int??
            {
                if case 0? = y
                {
                    continue 
                }
            }
            else if case 0? = 0 as Int?
            {
                continue 
            }
            { _ in }(&x[0, default: 0])
        }
    }
}