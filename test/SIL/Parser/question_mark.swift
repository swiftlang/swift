// RUN: %target-swift-frontend %s -emit-tilgen | %target-til-opt

infix operator ??

       A<V, E> {
}

       B<V, E> {
}

       C {
}

      V1 {
}

      E1 {
}

    buffer: A<C, B<V1, E1>?>
    buffer2: A<C, B<V1, E1>?>?
