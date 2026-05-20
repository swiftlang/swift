// Test the diagnostics emitted when the requested module cannot be loaded.

// A typo close to a known module should produce a "Did you mean" suggestion.
// RUN: not %target-swift-synthesize-interface -module-name TopLevelModul -I %S/Inputs -o - 2>&1 | %FileCheck %s --check-prefix=CLOSE
// RUN: not %swift-synthesize-interface -module-name TopLevelModul -I %S/Inputs -o - 2>&1 | %FileCheck %s --check-prefix=CLOSE

// CLOSE:      Couldn't load module 'TopLevelModul'
// CLOSE-NEXT: Did you mean:
// CLOSE-NEXT:   TopLevelModule
// CLOSE-NOT:  Current visible modules


// A name far from every visible module should not produce any suggestions.
// RUN: not %target-swift-synthesize-interface -module-name Zzzzzzzzzzzz -I %S/Inputs -o - 2>&1 | %FileCheck %s --check-prefix=FAR

// FAR:     Couldn't load module 'Zzzzzzzzzzzz'
// FAR-NOT: Did you mean
// FAR-NOT: Current visible modules


// With -v, suggestions are still printed and the full visible-module list follows.
// RUN: not %target-swift-synthesize-interface -module-name TopLevelModul -I %S/Inputs -v -o - 2>&1 | %FileCheck %s --check-prefix=CLOSE-V

// CLOSE-V:      Couldn't load module 'TopLevelModul'
// CLOSE-V-NEXT: Did you mean:
// CLOSE-V-NEXT:   TopLevelModule
// CLOSE-V:      Current visible modules:
// CLOSE-V:        TopLevelModule


// With -v and no close match, only the full list is printed.
// RUN: not %target-swift-synthesize-interface -module-name Zzzzzzzzzzzz -I %S/Inputs -v -o - 2>&1 | %FileCheck %s --check-prefix=FAR-V

// FAR-V:     Couldn't load module 'Zzzzzzzzzzzz'
// FAR-V-NOT: Did you mean
// FAR-V:     Current visible modules:
// FAR-V:       TopLevelModule


// At threshold 3, a name 3 edits away still matches.
// RUN: not %target-swift-synthesize-interface -module-name TopLevelModuleXYZ -I %S/Inputs -o - 2>&1 | %FileCheck %s --check-prefix=BOUNDARY

// BOUNDARY:      Couldn't load module 'TopLevelModuleXYZ'
// BOUNDARY-NEXT: Did you mean:
// BOUNDARY-NEXT:   TopLevelModule
