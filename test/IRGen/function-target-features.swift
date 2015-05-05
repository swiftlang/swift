// This test verifies that we produce target-cpu and target-features attributes
// on functions.

// RUN: %target-swift-frontend -target x86_64-apple-macosx10.10 -emit-ir -o - %s -Xcc -Xclang -Xcc -target-feature -Xcc -Xclang -Xcc +avx | FileCheck %s -check-prefix=AVX-FEATURE
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.10 -emit-ir -o - %s -Xcc -Xclang -Xcc -target-feature -Xcc -Xclang -Xcc +avx512f -Xcc -Xclang -Xcc -target-feature -Xcc -Xclang -Xcc +avx512er | FileCheck %s -check-prefix=TWO-AVX
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.10 -emit-ir -o - %s -Xcc -Xclang -Xcc -target-cpu -Xcc -Xclang -Xcc corei7 | FileCheck %s -check-prefix=CORE-CPU
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.10 -emit-ir -o - %s -Xcc -Xclang -Xcc -target-cpu -Xcc -Xclang -Xcc corei7 -Xcc -Xclang -Xcc -target-feature -Xcc -Xclang -Xcc +avx | FileCheck %s -check-prefix=CORE-CPU-AND-FEATURES
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.10 -emit-ir -o - %s -Xcc -Xclang -Xcc -target-cpu -Xcc -Xclang -Xcc x86-64 | FileCheck %s -check-prefix=X86-64-CPU
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.10 -emit-ir -o - %s -Xcc -Xclang -Xcc -target-cpu -Xcc -Xclang -Xcc corei7-avx -Xcc -Xclang -Xcc -target-feature -Xcc -Xclang -Xcc -avx | FileCheck %s -check-prefix=AVX-MINUS-FEATURE

func test() {
}

// AVX-FEATURE: "target-features"{{.*}}+avx
// TWO-AVX: "target-features"=
// TWO-AVX-DAG: +avx512er
// TWO-AVX-DAG: +avx512f
// CORE-CPU: "target-cpu"="corei7"
// CORE-CPU-AND-FEATURES: "target-cpu"="corei7" "target-features"={{.*}}+avx
// X86-64-CPU: "target-cpu"="x86-64"
// AVX-MINUS-FEATURE: "target-features"={{.*}}-avx
