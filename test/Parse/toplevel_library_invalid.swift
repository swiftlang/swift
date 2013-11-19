// RUN: %swift -parse -verify -parse-as-library %s

println("a"); // expected-error {{expressions are not allowed at the top level}} \
              // expected-error {{';' statements are not allowed}}
println("a");
