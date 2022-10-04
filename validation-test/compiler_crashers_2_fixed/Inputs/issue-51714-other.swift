struct S1: P1 {
    typealias P1_T1 = Int
    typealias P1_T2 = Range<Int>
}

struct S2<P1_T1>: P1, P3 {
    typealias P1_T2 = [P1_T1]
    typealias P2_T = Int
    typealias P3_T = S1
}
