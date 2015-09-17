This directory holds the Swift Standard Library's Glibc Module, comprised of
- The *overlay* library, which amends some APIs imported from Glibc, and
- The clang [module map](http://clang.llvm.org/docs/Modules.html) which
  specifies which headers need to be imported from Glibc for bare minimum
  functionality.
