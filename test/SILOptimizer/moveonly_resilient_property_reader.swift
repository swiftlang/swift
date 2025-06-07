// RUN: %target-swift-frontend -enable-library-evolution -Xllvm -sil-print-types -emit-sil %s | %FileCheck %s

public struct ResilientMemberC {}
public struct ResilientMemberNC: ~Copyable {}

public struct Resilient: ~Copyable {
    // CHECK-LABEL: sil @${{.*}}9pubPropNC{{.*}}vr :
    // CHECK:         [[BASE:%.*]] = load {{.*}} : $*Resilient
    // CHECK:         [[PROJ_BUF:%.*]] = alloc_stack $ResilientMemberNC
    // CHECK:         [[PROJ:%.*]] = struct_extract [[BASE]]
    // CHECK:         store [[PROJ]] to [[PROJ_BUF]]
    // CHECK:         yield [[PROJ_BUF]]
    public var pubPropNC: ResilientMemberNC = ResilientMemberNC()
}

