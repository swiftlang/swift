// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

public protocol Applicative {}

public struct Kind<X, Y> {}

public extension Applicative {
    static func product<A1,B> (_ fa:Kind<Self,A1>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,B)> {
        fatalError()
    }

    static func product<A1,A2,B> (_ fa:Kind<Self,(A1,A2)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,B> (_ fa:Kind<Self,(A1,A2,A3)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,B> (_ fa:Kind<Self,(A1,A2,A3,A4)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26,A27,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26,A27)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26,A27,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26,A27,A28,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26,A27,A28)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26,A27,A28,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26,A27,A28,A29,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26,A27,A28,A29)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26,A27,A28,A29,B)> {
        fatalError()
    }

    static func product<A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26,A27,A28,A29,A30,B> (_ fa:Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26,A27,A28,A29,A30)>, _ fb:Kind<Self,B>) -> Kind<Self,(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26,A27,A28,A29,A30,B)> {
        fatalError()
    }
}

public extension Applicative {
    static func zip<A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26,A27,A28,A29> (_ fa0:Kind<Self,A0>, _ fa1:Kind<Self,A1>, _ fa2:Kind<Self,A2>, _ fa3:Kind<Self,A3>, _ fa4:Kind<Self,A4>, _ fa5:Kind<Self,A5>, _ fa6:Kind<Self,A6>, _ fa7:Kind<Self,A7>, _ fa8:Kind<Self,A8>, _ fa9:Kind<Self,A9>, _ fa10:Kind<Self,A10>, _ fa11:Kind<Self,A11>, _ fa12:Kind<Self,A12>, _ fa13:Kind<Self,A13>, _ fa14:Kind<Self,A14>, _ fa15:Kind<Self,A15>, _ fa16:Kind<Self,A16>, _ fa17:Kind<Self,A17>, _ fa18:Kind<Self,A18>, _ fa19:Kind<Self,A19>, _ fa20:Kind<Self,A20>, _ fa21:Kind<Self,A21>, _ fa22:Kind<Self,A22>, _ fa23:Kind<Self,A23>, _ fa24:Kind<Self,A24>, _ fa25:Kind<Self,A25>, _ fa26:Kind<Self,A26>, _ fa27:Kind<Self,A27>, _ fa28:Kind<Self,A28>, _ fa29:Kind<Self,A29>) -> Kind<Self,(A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26,A27,A28,A29)> {
        product(product(product(product(product(product(product(product(product(product(product(product(product(product(product(product(product(product(product(product(product(product(product(product(product(product(product(product(product(fa0, fa1), fa2), fa3), fa4), fa5), fa6), fa7), fa8), fa9), fa10), fa11), fa12), fa13), fa14), fa15), fa16), fa17), fa18), fa19), fa20), fa21), fa22), fa23), fa24), fa25), fa26), fa27), fa28), fa29) // Ok
    }
}
