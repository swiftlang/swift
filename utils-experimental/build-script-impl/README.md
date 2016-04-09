# build_script_impl

`build_script_impl` holds implementation of Swift build script.

## Tests

You may run unit tests for `build-script` from the command line:

```sh
apple/swift $ python -m unittest discover utils/build-script-impl
```

## Developer notes

- `build-script` : the launcher
- `build-script-impl/`
  - `build_script/` : package
    - `__main__.py` : main entry point
    - `main_normal.py` : the main work flow
    - `workspace.py` : `workspace` represents whole source tree.
    - `host/` : `host` represents running machine itself. Subclassed depends on
      the machine.
    - `cmake.py` : represents cmake command.
    - `products/` : represents each product to be built.
    - `shell.py` : every shell command invocation must be done via this module.
    - `utils/` : some utility functions
    - `main_preset.py` : sub entry point when `__main__.py` is called with
      `--preset`
  - `tests/` : TESTS
