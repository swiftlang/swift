This directory holds the Swift Standard Library's Darwin and Glibc Module,
comprised of

- The *overlay* library, which amends some APIs imported from Clang module.
- Glibc only: The clang [module map] which specifies which headers need to
  be imported from Glibc for bare minimum functionality.

Note: On Darwin platforms, we assume clang [module map] is already installed
in the system SDK.

[module map]: http://clang.llvm.org/docs/Modules.html
