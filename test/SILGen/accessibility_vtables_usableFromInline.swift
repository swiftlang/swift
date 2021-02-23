// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-silgen %S/Inputs/accessibility_vtables_usableFromInline_helper.swift | %FileCheck %s --check-prefix=LIBRARY
// RUN: %target-swift-frontend -enable-library-evolution -emit-silgen %S/Inputs/accessibility_vtables_usableFromInline_helper.swift | %FileCheck %s --check-prefix=LIBRARY

// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/accessibility_vtables_usableFromInline_helper.swift
// RUN: %target-swift-emit-silgen -primary-file %s -I %t | %FileCheck %s --check-prefix=FRAGILE-CLIENT

// RUN: %target-swift-frontend -enable-library-evolution -emit-module -o %t %S/Inputs/accessibility_vtables_usableFromInline_helper.swift
// RUN: %target-swift-emit-silgen -primary-file %s -I %t | %FileCheck %s --check-prefix=RESILIENT-CLIENT

import accessibility_vtables_usableFromInline_helper

public class Derived : Middle {
  open override func internalMethod() {}
  open override func usableFromInlineMethod() {}
}

// LIBRARY-LABEL: sil_vtable {{(\[serialized\] )?}}Base {
// LIBRARY-NEXT:    #Base.internalMethod: (Base) -> () -> () : @$s45accessibility_vtables_usableFromInline_helper4BaseC14internalMethodyyF
// LIBRARY-NEXT:    #Base.usableFromInlineMethod: (Base) -> () -> () : @$s45accessibility_vtables_usableFromInline_helper4BaseC0cdE6MethodyyF
// LIBRARY-NEXT:    #Base.init!allocator: (Base.Type) -> () -> Base : @$s45accessibility_vtables_usableFromInline_helper4BaseCACycfC
// LIBRARY-NEXT:    #Base.deinit!deallocator: @$s45accessibility_vtables_usableFromInline_helper4BaseCfD
// LIBRARY-NEXT:  }

// LIBRARY-LABEL: sil_vtable {{(\[serialized\] )?}}Middle {
// LIBRARY-NEXT:    #Base.internalMethod: (Base) -> () -> () : @$s45accessibility_vtables_usableFromInline_helper6MiddleC14internalMethodyyFAA4BaseCADyyFTV [override]
// LIBRARY-NEXT:      #Base.usableFromInlineMethod: (Base) -> () -> () : @$s45accessibility_vtables_usableFromInline_helper6MiddleC0cdE6MethodyyF [override]
// LIBRARY-NEXT:      #Base.init!allocator: (Base.Type) -> () -> Base : @$s45accessibility_vtables_usableFromInline_helper6MiddleCACycfC [override]
// LIBRARY-NEXT:      #Middle.internalMethod: (Middle) -> () -> () : @$s45accessibility_vtables_usableFromInline_helper6MiddleC14internalMethodyyF
// LIBRARY-NEXT:      #Middle.deinit!deallocator: @$s45accessibility_vtables_usableFromInline_helper6MiddleCfD
// LIBRARY-NEXT:  }

// FRAGILE-CLIENT-LABEL: sil_vtable [serialized] Derived {
// FRAGILE-CLIENT-NEXT:    #Base.internalMethod: (Base) -> () -> () : @$s45accessibility_vtables_usableFromInline_helper6MiddleC14internalMethodyyFAA4BaseCADyyFTV [inherited]
// FRAGILE-CLIENT-NEXT:    #Base.usableFromInlineMethod: (Base) -> () -> () : @$s38accessibility_vtables_usableFromInline7DerivedC0cdE6MethodyyF [override]
// FRAGILE-CLIENT-NEXT:    #Base.init!allocator: (Base.Type) -> () -> Base : @$s45accessibility_vtables_usableFromInline_helper6MiddleCACycfC [inherited]
// FRAGILE-CLIENT-NEXT:    #Middle.internalMethod: (Middle) -> () -> () : @$s38accessibility_vtables_usableFromInline7DerivedC14internalMethodyyF [override]
// FRAGILE-CLIENT-NEXT:    #Derived.deinit!deallocator: @$s38accessibility_vtables_usableFromInline7DerivedCfD

// FRAGILE-CLIENT-NEXT:  }

// RESILIENT-CLIENT-LABEL: sil_vtable [serialized] Derived {
// RESILIENT-CLIENT-NEXT:    #Base.usableFromInlineMethod: (Base) -> () -> () : @$s38accessibility_vtables_usableFromInline7DerivedC0cdE6MethodyyF [override]
// RESILIENT-CLIENT-NEXT:    #Middle.internalMethod: (Middle) -> () -> () : @$s38accessibility_vtables_usableFromInline7DerivedC14internalMethodyyF [override]
// RESILIENT-CLIENT-NEXT:    #Derived.deinit!deallocator: @$s38accessibility_vtables_usableFromInline7DerivedCfD
// RESILIENT-CLIENT-NEXT:  }
