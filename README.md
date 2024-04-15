# SML C Lexer

This is a C lexer written in SML.

The goal of this project is to implement the entirety of the C
programming language syntax.

I cannot pretend that there is a practical use to this. There is
none. I did this to educate myself and refine my skills.

# Usage

As of now, I'm using the SML/NJ interpreter for this since MLton
was giving me issues.

`sml cli.sml <program.c>`

You can run the example projects like so:
```bash
sml cli.sml examples/hello_world.c
sml cli.sml examples/loops.c
```

The program tokenizes the file then prints all of the tokens.
