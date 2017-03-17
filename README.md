# GolfASM

GolfASM is an eccentric, assembly-like programming language that also takes inspiration from BrainF**k. GolfASM syntax is designed such that programs are as short as possible given its assembly-like execution model.

There are four components to a GolfASM machine:
- The code buffer, from which instructions are interpreted
- The stack, a stack of expressions, initially empty
- The heap, an integer-indexed array of expressions
- The 26 registers named 'a' through 'z'. 'a' is the accumulator register
    used for various computations.

GolfASM memory contains expressions that can be integer, character, or list of integer, character or list.

## GolfASM Commands
- `{` copy the code up to the matching `}` on top of the stack
- `}` pop the code on top of the stack onto the execution buffer if the accumulator is 0, otherwise pop it to nowhere
- `(` push the code up to the matching `)` on top of the stack, and discard the matching `)`
- `+` add the two integers on top of the stack. Afterwards, the arguments are removed and the result is on top of the stack.
- etc
