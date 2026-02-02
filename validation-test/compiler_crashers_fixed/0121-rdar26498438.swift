// RUN: not %target-swift-frontend %s -typecheck

class C { }

protocol PI
{
    init()
}

protocol MDS
{
    associatedtype Index : PI
    func f(_ : MVC<Index, Self>, atIndex index : Index) -> C?
}

class MVC<Index : PI, DataSource : MDS>: C where DataSource.Index == Index
{
    
}

struct LPI : PI
{
    var x : Int
    var y : Int
}

extension LPI
{
    init()
    {
        x = 0
        y = 0
    }
}


class LPVC: MVC<LPI, LPVC>
{
    
}
