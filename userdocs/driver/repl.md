# REPL Mode

The Swift REPL (Read-Eval-Print Loop) makes it easy to quickly run individual statements and expressions with instant feedback. To launch the REPL, run `swift` without providing any input files.

The REPL will evaluate any Swift code typed at the prompt and display the result immediately. You can press TAB at any time for code completion suggestions, and the up and down arrow keys can be used to access the REPL session's history. To run a swift script in the current REPL session, type `<` followed by its path.

Each REPL session is also a full-featured LLDB debugging session. Any line starting with `:` will be interpreted as an LLDB command. For a complete listing of LLDB commands, type `:help`.

To exit the REPL, type `:quit` or use CTRL-D.
