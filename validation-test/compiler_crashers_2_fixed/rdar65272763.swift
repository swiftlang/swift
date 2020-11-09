// RUN: %target-swift-frontend -emit-ir %s

public struct S1<T1> {}
public extension S1 where T1 == Int {
    public struct S2<T2> {
        let value: T2
        
        public init(value: T2) {
            self.value = value
        }
    }
    
    public init<T>(s: [S2<T>]) {
        self.init()
        
        s.forEach { _ in
            
        }
    }
}

