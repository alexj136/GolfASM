# GolfASM

GolfASM is a stack machine programming language like assembly, but with
single-character commands.

Uppercase alphabetical characters pop the stack to a register, and the lowecase
corresponding character pushes the register on the stack.

Data types: Int, Char, List. Lists can contain data or commands.

Other commands - each take one or two arguments on the stack and remove them
    - binary integer ops: `+-*/%`
    - list ops: `|` - head, `#` - tail, `:` - concat
    - print with `$`
    - `.` takes a list of commands on the stack and puts them at the start of
      the code buffer, effectively calling that code
    - Lists delimited with `[` and `]` - they must match 
    - `?` ternary operator
    - integer and string literals e.g. `1232`, "ashbdj"
    - whitespace is ignored but separates multi-char tokens (i.e. int literals)
