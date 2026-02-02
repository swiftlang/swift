// RUN: %target-swift-frontend %s -disable-availability-checking -emit-ir

func f<let i: Int, T>(
    _: consuming InlineArray<i, (String, T)>
) {
}
