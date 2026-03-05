// Helper file with a deliberate type error triggered when another file
// references HelperWrapper (causing the compiler to type-check this file's
// body during the primary file's type-checking session).
struct HelperWrapper {
    // References NonExistentHelperType so that type-checking this struct body
    // produces an error.
    private let x: NonExistentHelperType = ()
}
